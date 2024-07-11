#' quantnorm
#'
#' quantnorm applies quantile normalization to a variable and returns the dataframe with a column of normalized values.
#'
#' @param df A dataframe that will be processed.
#' @param x The variable that will undergo quantile normalization
#' @return Returns a dataframe with an additional column with the normalized values.
#' @export
#' @import forecast
#' @import stats
#' @import dplyr


quantnorm <- function(df, x) {

  x <- as.character(substitute(x)) # Convert variable names to characters for later subsetting

  values <- df[[x]]

  rank_values <- rank(values, ties.method = "min")
  sorted_values <- sort(values)
  quantiles <- quantile(sorted_values, probs = (rank_values - 1) / (length(values) - 1))

  df[[paste("norm", x)]] <- quantiles[rank_values]

  return(df)
}
