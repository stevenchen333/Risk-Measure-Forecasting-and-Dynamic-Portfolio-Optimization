get_stock_prices <- function(tickers, start_date, end_date, output_file = NULL) {
  # Load required packages
  if (!requireNamespace("quantmod", quietly = TRUE)) {
    stop("Please install the quantmod package: install.packages('quantmod')")
  }
  if (!requireNamespace("zoo", quietly = TRUE)) {
    stop("Please install the zoo package: install.packages('zoo')")
  }

  # Convert dates to Date objects
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  # Initialize empty list to store data
  price_list <- list()

  # Download data for each ticker
  for (ticker in tickers) {
    message(paste("Downloading", ticker, "data..."))

    stock_data <- tryCatch(
      {
        quantmod::getSymbols(
          Symbols = ticker,
          src = "yahoo",
          from = start_date,
          to = end_date,
          auto.assign = FALSE
        )
      },
      error = function(e) {
        warning(paste("Failed to download data for", ticker, ":", e$message))
        return(NULL)
      }
    )

    if (!is.null(stock_data)) {
      # Extract adjusted prices
      prices <- data.frame(
        Date = zoo::index(stock_data),
        Ticker = ticker,
        Adjusted = as.numeric(stock_data[, paste0(ticker, ".Adjusted")]),
        stringsAsFactors = FALSE
      )

      price_list[[ticker]] <- prices
    }
  }

  # Combine all data frames
  if (length(price_list) == 0) {
    stop("No data was successfully downloaded for any ticker")
  }

  combined_df <- do.call(rbind, price_list)

  # Reshape to wide format (optional - comment out if you prefer long format)
  wide_df <- reshape(combined_df,
                     idvar = "Date",
                     timevar = "Ticker",
                     direction = "wide")

  # Clean up column names in wide format
  colnames(wide_df) <- gsub("Adjusted.", "", colnames(wide_df))

  # Save to CSV if output file specified
  if (!is.null(output_file)) {
    write.csv(wide_df, file = output_file, row.names = FALSE)
    message(paste("Data saved to", output_file))
  }

  # Return the wide format data frame
  return(wide_df)
}




stock_list = c("AAPL", "MSFT", "META", "V", "JPM",
               "C", "BTC-USD","UEC", "GM", "ET")
get_stock_prices(stock_list, start_date= "2020-01-01", end_date = "2021-12-31", output_file = "stocks_adj_prices1.csv" )
get_stock_prices(stock_list, start_date= "2022-01-01", end_date = "2025-01-01", output_file = "stocks_adj_prices2.csv" )
