# Train-test split function


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



basic_eda<- function(series,cutoff = NULL,lag, x_1 = NULL, y_1 = NULL, x_2 = NULL, y_2 = NULL){
  layout_matrix <- matrix(c(1, 1,
                            2, 3),
                          nrow = 2, byrow = TRUE)

  layout(layout_matrix)

  ts.plot(series)
  if (is.null(x_1) == FALSE & is.null(y_1) == FALSE& is.null(x_2) == FALSE& is.null(y_2) == FALSE & is.null(cutoff) == FALSE){
    abline(v = cutoff, col = "red", lty = 2)
    text(x_1,y_1, "training data")
    text(x_2,y_2, "test data")}


  acf(series, lag);pacf(series,lag)


  par(mfrow = c(1,1))
}


calculate_returns <- function(stock_df, type = c("log", "simple")) {
  type <- match.arg(type)

  # Extract dates and prices
  dates <- index(stock_df)
  prices <- stock_df

  # Compute returns
  if (type == "log" ) {
    returns <- diff(log(as.matrix(prices)))
  } else {
    returns <- diff(as.matrix(prices)) / head(as.matrix(prices), -1)
  }

  # Adjust dates to match the return periods
  return_dates <- dates[-1]

  # Combine into a new data frame
  return_df <- data.frame(returns)
  colnames(return_df) <- colnames(stock_df)
  return(return_df)
}



