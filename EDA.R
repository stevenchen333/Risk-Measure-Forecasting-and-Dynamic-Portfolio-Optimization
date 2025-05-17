


require(zoo)
require(forecast)

stock_df = read.csv("stocks_adj_prices")[1:753,]

head(stock_df)
tail(stock_df)
#View(stock_df)
#unique(is.na.data.frame(stock_df)) #no NA




rownames(stock_df)<-stock_df[,1];stock_df<-stock_df[,-1]


stock_df = zoo(stock_df, order.by = rownames(stock_df))



#---
# Plots


# Set up plotting area for 10 plots, e.g., 5 rows x 2 cols
par(mfrow = c(5, 2), mar = c(3, 3, 2, 1))  # adjust margins as needed

# Plot each normalized series separately
for (i in 1:10) {
  ts.plot(stock_df[, i], main = colnames(stock_df)[i], ylab = "Normalized", xlab = "Time", col = "blue")
}

# Reset plotting layout back to default
par(mfrow = c(1, 1))




# Normalize each column by scaling to [0, 1]
normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply normalization to each stock (each column)
stock_norm <- apply(stock_df, 2, normalize)

# Plot all normalized series in one plot
matplot(stock_norm, type = "l", lty = 1, col = rainbow(ncol(stock_df)),
        xlab = "Time", ylab = "Normalized Price",
        main = "Normalized Stock Prices Over Time")

# Add legend
legend("bottomright", legend = colnames(stock_df), col = rainbow(ncol(stock_df)), lty = 1, cex=0.7)

#---


calculate_returns <- function(stock_df, type = c("log", "simple")) {
  type <- match.arg(type)

  # Extract dates and prices
  dates <- index(stock_df)
  prices <- stock_df

  # Compute returns
  if (type == "log") {
    returns <- diff(log(as.matrix(prices)))
  } else {
    returns <- diff(as.matrix(prices)) / head(as.matrix(prices), -1)
  }

  # Adjust dates to match the return periods
  return_dates <- dates[-1]

  # Combine into a new data frame
  return_df <- data.frame(returns)
  colnames(return_df) <- colnames(prices)

  return(return_df)
}


returns_df = calculate_returns(stock_df, "log")



# Set up plotting area for 10 plots, e.g., 5 rows x 2 cols
par(mfrow = c(5, 2), mar = c(3, 3, 2, 1))  # adjust margins as needed

# Plot each normalized series separately
for (i in 1:10) {
  ts.plot(returns_df[, i], main = colnames(stock_df)[i], ylab = "Normalized", xlab = "Time", col = "blue")
  abline(h = mean(returns_df[, i]), col = "red", lty = 2)
}

# Reset plotting layout back to default
par(mfrow = c(1, 1))

# Plot all normalized series in one plot
matplot(returns_df, type = "l", lty = 1, col = rainbow(ncol(stock_df)),
        xlab = "Time", ylab = "Normalized Price",
        main = "Normalized Stock Prices Over Time")

# Add legend
legend("bottomright", legend = colnames(stock_df), col = rainbow(ncol(stock_df)), lty = 1, cex=0.7)


#----

#ACF of log returns

acf(returns_df)
pacf(returns_df)

#-----

# squared returns
# Summary statistics for each stock's returns
summary_stats <- sapply(returns_df, function(x) {
  c(Mean = mean(x),
    SD = sd(x),
    Min = min(x),
    Max = max(x),
    Skewness = moments::skewness(x),
    Kurtosis = moments::kurtosis(x))
})

print(round(summary_stats, 4))




# Plot squared returns (proxy for volatility) for each stock
par(mfrow = c(5, 2), mar = c(3, 3, 2, 1))
for (i in 1:10) {
  plot(returns_df[,i]^2, type = "h", main = paste("Squared Returns:", colnames(returns_df)[i]),
       ylab = "Squared Returns", xlab = "Time", col = "blue")
}
par(mfrow = c(1, 1))







# Histograms with normal density overlay
par(mfrow = c(5, 2), mar = c(3, 3, 2, 1))
for (i in 1:10) {
  hist(returns_df[,i], breaks = 30, freq = FALSE,
       main = paste("Return Distribution:", colnames(returns_df)[i]),
       xlab = "Log Returns")
  curve(dnorm(x, mean = mean(returns_df[,i]), sd = sd(returns_df[,i])),
        add = TRUE, col = "red", lwd = 2)
}
par(mfrow = c(1, 1))



acf(returns_df^2)
pacf(returns_df^2)

#-------

