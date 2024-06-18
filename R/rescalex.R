#' rescalex
#'
#' rescalex rescales variables to be on a scale from 0 to 1. This is useful for examining date variables and when events occur
#' within the observation period.
#'
#' @param df A dataframe that will be processed.
#' @param x Numeric vector that will be rescaled to be from 0 to 1.
#' @param varname The variable name for the rescaled variable (default is "scalex")
#' @return Returns a dataframe that contains an additional column with the rescaled x variable.
#' @export
#' @import stats
#' @import dplyr
#' @import ggplot2

rescalex <- function(df, x, varname = "scalex"){

  x <- as.character(substitute(x)) # Convert variable names to characters for later subsetting

  varname <- as.character(substitute(varname))

  minx <- min(as.numeric(df[[x]])) # Find minimum of variable x

  maxx <- max(as.numeric(df[[x]])) # Find maximum of variable x

  xrange <- as.numeric(maxx - minx) # Create range by subtracting minimum from maximum

  df[[varname]] <- as.numeric(df[[x]] - minx) / xrange # Create scaled variable by subtracting minimum from each observation and dividing by range

  return(df)

}
