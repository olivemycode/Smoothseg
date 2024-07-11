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


quantnorm <- function(df, variable_name) {
  # Extract the variable as a matrix
  x <- as.matrix(df[[variable_name]])

  # Rank the data and compute the quantiles
  rank_x <- apply(x, 2, rank, ties.method = "min")
  sorted_x <- apply(x, 2, sort)
  mean_quantiles <- rowMeans(sorted_x)

  # Normalize the data
  normalized_x <- x
  for (i in 1:ncol(x)) {
    normalized_x[, i] <- mean_quantiles[rank_x[, i]]
  }

  # Replace the variable in the dataframe with the quantile-normalized values
  df[[variable_name]] <- as.numeric(normalized_x)

  return(df)
}
