# Train-test split function
require(aTSA)
library(fBasics)
require(zoo)
require(forecast)
library(FinTS)
library(rugarch)
library(ggplot2)
library(tidyr)


train_test<- function(df, cutoff = "2024-01-01"){
  index = which(index(stock_df) >= as.Date(cutoff))
  stock_df_train<- stock_df[-index,]
  stock_df_train<- zoo(stock_df_train, order.by = rownames(stock_df_train))


  stock_df_test <- stock_df[index,]
  stock_df_test<- zoo(stock_df_test, order.by = rownames(stock_df_test))


  return(list(
    train = stock_df_train,
    test = stock_df_test
  ))
}




# Stationarity test adf test and kpss test

stationarity_tests<- function(series, nlag = 6){
  series<- ts(series)
  adf<-adf.test(as.matrix(series), nlag = nlag, output = T)
  print("----------------------------------------------")
  print("----------------------------------------------")
  kpss<- kpss.test(as.matrix(series), output = T)

}



# ACF PACF of returns and return^2
acf_pacf_ret<-function(returns, lag){
  par(mfrow = c(2,2))
  acf(ts(returns), lag.max =lag);pacf(ts(returns), lag.max = lag)
  acf(ts(returns)^2, lag.max = lag);pacf(ts(returns)^2, lag.max = lag)
  par(mfrow = c(1,1))
}




arch_test<- function(series, order = NULL){
  if (is.null(order)){order = c(0,0,0)}

  archtest_lags <- list()

  for (i in 1:12) {
    archtest_lags[[i]] <- ArchTest(arima(aapl_return_train, order = order, include.mean = FALSE)$residuals, lags = i)
  }

  # Extract lags, statistics, and p-values
  arch_df <- data.frame(
    Lag = 1:12,
    Statistic = sapply(archtest_lags, function(x) x$statistic),
    P_Value = sapply(archtest_lags, function(x) round(x$p.value, 4))
  )

  # View the result
  print(arch_df)

}




arma_garch_ic_plot <- function(return_series,
                               garch_spec = list(model = "sGARCH", garchOrder = c(1, 1)),
                               arma_p_range = 0:4,
                               arma_q_range = 0:3,
                               distribution = "norm",
                               include_mean = FALSE,
                               output = "plot",
                               verbose = FALSE) {

  # Validate input
  if (!is.data.frame(return_series) && !is.xts(return_series) && !is.numeric(return_series)) {
    stop("return_series must be a dataframe, xts object, or numeric vector")
  }

  # Get series name for labeling
  series_name <- ifelse(is.data.frame(return_series),
                        colnames(return_series)[1],
                        deparse(substitute(return_series)))

  # Convert to numeric if needed
  if (is.data.frame(return_series) || is.xts(return_series)) {
    return_series <- as.numeric(return_series[,1])
  }

  # Storage for results
  fitted_models <- list()
  info_criteria <- list()
  failed_models <- list()
  best_ic <- list(value = Inf, model = NULL, order = c(NA, NA))

  # Loop through ARMA(p,q) combinations
  for (p in arma_p_range) {
    for (q in arma_q_range) {
      model_name <- paste0("ARMA(", p, ",", q, ")")

      if (verbose) {
        cat("Fitting:", model_name, "\n")
      }

      # GARCH spec
      spec <- ugarchspec(
        variance.model = garch_spec,
        mean.model = list(armaOrder = c(p, q), include.mean = include_mean),
        distribution.model = distribution
      )

      # Fit model with error handling
      fit <- tryCatch(
        {
          fit_result <- ugarchfit(spec, return_series, solver = "hybrid")

          # Check convergence
          if (fit_result@fit$convergence != 0) {
            stop("Convergence not achieved")
          }
          fit_result
        },
        error = function(e) {
          if (verbose) message("Failed to fit ", model_name, ": ", e$message)
          NULL
        }
      )

      if (!is.null(fit)) {
        fitted_models[[model_name]] <- fit
        ic <- infocriteria(fit)
        info_criteria[[model_name]] <- ic

        # Track best model (using AIC)
        if (ic["Akaike",] < best_ic$value) {
          best_ic$value <- ic["Akaike",]
          best_ic$model <- model_name
          best_ic$order <- c(p, q)
        }
      } else {
        failed_models[[model_name]] <- c(p, q)
      }
    }
  }

  # Create info criteria dataframe
  ic_df <- do.call(rbind, lapply(names(info_criteria), function(name) {
    data.frame(Model = name, t(as.data.frame(info_criteria[[name]])))
  }))
  rownames(ic_df) <- ic_df$Model

  # Convert to long format for ggplot
  ic_long <- pivot_longer(ic_df, cols = -Model, names_to = "Criterion", values_to = "Value")

  # Plot
  plot <- ggplot(ic_long, aes(x = Model, y = Value, color = Criterion, group = Criterion)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    theme_minimal() +
    labs(title = paste("Information Criteria for ARMA-GARCH Models:", series_name),
         x = "Model",
         y = "Criterion Value") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_text(data = subset(ic_long, Criterion == "Akaike" & Value == min(Value)),
              aes(label = "Best AIC"), vjust = -1, color = "black")

  # Return based on output option
  if (output == "all") {
    return(list(
      models = fitted_models,
      info_criteria = ic_df,
      plot = plot,
      best_model = best_ic,
      failed_models = failed_models,
      series_name = series_name
    ))
  } else if (output == "best") {
    return(best_ic)
  } else {
    print(plot)  # Display the plot
    return(plot)  # Return it as well
  }
}

arma_garch_cv <- function(data, p_range = 0:4, q_range = 0:3,
                          window_size = 300, n_rolls = 10, dist_model = "norm") {
  cv_results <- list()
  fitted_models <- list()

  for (p in p_range) {
    for (q in q_range) {
      model_name <- paste0("ARMA(", p, ",", q, ")")

      mse <- mae <- loglik <- aic_vec <- bic_vec <- numeric(n_rolls)

      for (i in 1:n_rolls) {
        train_start <- i
        train_end <- window_size + i - 1
        train_data <- data[train_start:train_end]
        test_data <- data[train_end + 1]

        spec <- ugarchspec(
          variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
          mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
          distribution.model = dist_model
        )

        fit <- tryCatch(
          ugarchfit(spec, data = train_data, solver = "hybrid", fit.control = list(scale = 1)),
          error = function(e) NULL
        )

        if (!is.null(fit)) {
          forecast <- ugarchforecast(fit, n.ahead = 1)
          pred_mean <- fitted(forecast)[1]
          sigma_t <- sigma(forecast)[1]

          mse[i] <- (test_data - pred_mean)^2
          mae[i] <- abs(test_data - pred_mean)
          loglik[i] <- -dnorm(test_data, mean = pred_mean, sd = sigma_t, log = TRUE)

          info_crit <- infocriteria(fit)
          aic_vec[i] <- info_crit[1]
          bic_vec[i] <- info_crit[2]
        } else {
          mse[i] <- mae[i] <- loglik[i] <- aic_vec[i] <- bic_vec[i] <- NA
        }
      }

      cv_results[[model_name]] <- data.frame(
        MSE = mse,
        MAE = mae,
        NegLogLik = loglik,
        AIC = aic_vec,
        BIC = bic_vec
      )

      fitted_models[[model_name]] <- spec
    }
  }

  return(list(
    results = cv_results,
    models = fitted_models
  ))
}




