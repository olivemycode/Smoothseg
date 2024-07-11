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

  data <- as.matrix(df[[x]]) # convert numbers into a matrix

  ranks <- apply(data, 2, rank, ties.method = "min") # find ranks of the data

  sorted_data <- apply(data, 2, sort)

  row_means <- rowMeans(sorted_data) # sort data and get average of each row

  normalized_data <- matrix(0, nrow = nrow(data), ncol = ncol(data)) # replace value with mean of values of the sma rank

  for (i in 1:ncol(data)) {
    normalized_data[, i] <- row_means[ranks[, i]]
  }

  return(normalized_data)
}
