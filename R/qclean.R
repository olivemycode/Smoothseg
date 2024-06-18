#' qclean
#'
#' qclean converts date variables to POSIXct, aggregates and orders data by a specified variable, removes specified quantiles,
#' and creates an index column for later kernel or indexing purposes.
#'
#' @param df A dataframe that will be processed.
#' @param x The variable to aggregate y by.
#' @param y The variable to be aggregated.
#' @param a Lower-bound percentile of observations to be removed (set to 0 by default).
#' @param b Upper-bound percentile of observations to be removed (set to 1 by default).
#' @param date If date is TRUE, converts x variable to POSIXct for numerical analysis (set to TRUE by default).
#' @return Returns a dataframe that is ordered and aggregated by x with percentiles below a and above b removed.
#' @export
#' @import stats
#' @import dplyr
#' @import ggplot2
#' @import zoo

qclean <- function(df, x, y, a = 0, b = 1, date = TRUE){

  x <- as.character(substitute(x)) # Convert variable names to characters for later subsetting

  y <- as.character(substitute(y))

  if (date == TRUE){
    df[[x]] <- as.POSIXct(df[[x]], format = "%m/%d/%Y") # If the input variable is a date, convert to POSIXct for numerical analysis
  }

  aggregatex <- aggregate(df[[y]], by = list(df[[x]]), FUN = function(x) sum(x, na.rm = TRUE)) # Aggregate by summing y by x and removing NAs

  colnames(aggregatex) <- c(x, y) # Rename variables in aggregated table to original variable names

  ordereddf <- aggregatex[order(aggregatex[[x]]), ] # Order data by descending x

  qcase <- ordereddf[ordereddf[[y]] < quantile(ordereddf[[y]], b) & ordereddf[[y]] > quantile(ordereddf[[y]], a), ] # Remove quantiles specified in function

  Index <- seq.int(nrow(qcase)) # Create Index column for kernel and indexing purposes

  qcase <- cbind(Index = Index, qcase)

  return(qcase) # Return cleaned dataset
}
