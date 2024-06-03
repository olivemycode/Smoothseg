#' ma
#'
#' ma creates simple moving averages and exponential moving averages based on parameters specified by the user. It can handle date variables.
#'
#' @param df A dataframe that will be processed.
#' @param x The variable to average y over.
#' @param y The variable to be averaged.
#' @param alpha Smoothing parameter for the exponential moving average. A higher alpha places more emphasis on the current observation while
#' a lower alpha places more emphasis on previous observations (set to 1 by defauilt).
#' @param n Number of observations taken into account by the moving average algorithms (set to 7 by default).
#' @param sma If sma is true, calculates the simple moving average and stores it in a variable named "sma" (set to TRUE by default).
#' @param ema If ema is true, calculates the exponential moving average and stores it in a variable named "ema" (set to TRUE by default).
#' @param date If date is TRUE, converts x variable to Date for calculating moving averages (set to TRUE by default).
#' @param u Units for the x variable to calculate time interval (set to "days" by default).
#' @return Returns a dataframe that has columns for simple moving average and exponential moving average based on specified parameters.
#' @export
#' @import stats
#' @import dplyr
#' @import ggplot2
#' @import zoo

ma <- function(df, x, y, alpha = 1, n = 7, sma = TRUE, ema = TRUE, date = TRUE, u = "days"){

  x <- as.character(substitute(x)) # Convert variable names to characters for later subsetting

  y <- as.character(substitute(y))

  if (date == TRUE){ # If x is intended to be a date, such as a POSIXct or character, this should be left as TRUE
    df[[x]] <- as.Date(df[[x]], format = "%m/%d/%Y")
  }

  if (sma == TRUE){ # Creates a simple moving average if sma is TRUE
    sma_values <- numeric(nrow(df)) # Creates a numeric list the same length as the dataframe

    for (i in 1:nrow(df)) {
      start_index <- max(1, i - n + 1) # Determine the start and end indices for the rolling window

      end_index <- i

      time_interval <- as.numeric(df[end_index, x] - df[start_index, x], units = u) # Calculate the time interval

      sum_values <- sum(df[start_index:end_index, y]) # Calculate the sum of values within the rolling window

      sma_values[i] <- sum_values / (time_interval + 1) # Calculate the rolling simple moving average

      df$sma <- sma_values # Create new column for simple moving average values
    }
  }

  if (ema == TRUE){ # Creates an exponential moving average if ema is TRUE (only influenced by prior observations)
    ema_values <- numeric(nrow(df)) # Creates a numeric list the same length as the dataframe

    ema_values[1] <- df[[y]][1] # Set first exponential moving average value to be first observation

    for (i in 2:nrow(df)) {
      time_interval <- as.numeric(df[[x]][i] - df[[x]][i - 1], units = u) # Calculate the time interval between the current and previous dates

      adjusted_alpha <- 1 - exp(-alpha * time_interval) # Calculate and adjust the smoothing factor based on the time interval

      ema_values[i] <- adjusted_alpha * df[[y]][i] + (1 - adjusted_alpha) * ema_values[i - 1] # Calculate the exponential moving average using the previous value and the current observation
    }

    df$ema <- ema_values # Create new column for exponential moving average values
  }

  return(df) # Return dataframe with new moving average columns
}
