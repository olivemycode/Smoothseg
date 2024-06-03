#' predictsegs
#'
#' predictsegs automatically detects changepoints in the data and returns an interactive plot with the predicted number of changepoints.
#' Users specify the maximum number of changepoints possible and number of sub-intervals in the data.
#'
#' @param df A dataframe that will be processed.
#' @param w Numeric or date vector that will become the x-axis for the final plot.
#' @param y Numeric response vector that is the same length as w.
#' @param k Numeric vector containing the max number of automatically-detected changepoints to be predicted.
#' @param z Method used for detecting number of changepoints in the plot (default set to "bic" but "aic" can be used)
#' @param g Number of user-specified sub-intervals to examine when searching for changepoints (default set to 1).
#' @param interest Vector containing any points of interest; these points of interest should be on the same scale as w.
#' @param col Color of line to indicate points of interest (default set to "red").
#' @param linet Type of vertical line to indicate points of interest (default set to "dashed").
#' @param date Boolean indicating if the points of interest are dates (default set to FALSE).
#' @return Returns a plotly interactive graph that shows the projected changepoint predictions.
#' @export
#' @import stats
#' @import dplyr
#' @import ggplot2
#' @import segmented
#' @import plotly
#' @import zoo

predictsegs <- function(df, w, y, k, z = "bic", g = 1, interest = c(), col = "red", linet = "dashed", date = FALSE){

  if (date == TRUE){
    interest <- as.Date(interest, format = "%m/%d/%Y") # Convert points of interest into a date if specified to be a date
  }

  x <- as.numeric(w) # Convert variable to numeric

  xname <- gsub(".*\\$", "", deparse(substitute(w))) # Extract variable name from input

  yname <- gsub(".*\\$", "", deparse(substitute(y)))

  k <- sort(k) # Sort max number of changepoints in ascending order

  predfit <- lm(y ~ 1 + x, data = df) # Run linear model using selected variables with an intercept

  predstorage <- list() # Create empty list to store dataframes

  changepoints <- list() # Create empty list to store number of predicted changepoints

  for (i in 1:length(k)){

    predseg <- selgmented(predfit, seg.Z = ~ x, Kmax = k[i], type = z, G = g) # Use selgmented function to predict changepoints with max k

    changepoints[i] <- (length(predseg$coefficients) - 2) / 2 # Calculate number of changepoints in the model for plotting

    predsegdf <- cbind(na.omit(df), predict(predseg)) # Combine non-missing values with predictions

    predsegdf <- predsegdf %>% rename(predseg = "predict(predseg)") # Rename predictions for easier reference

    predstorage[[i]] <- predsegdf # Store dataframes with predictions in list
  }

  predsegplot <- ggplot(predstorage[[1]], aes(x = w, y = y)) + # Initialize plot with original observations
    labs(x = xname, y = yname) +
    geom_point(size = 0.5) +
    geom_vline(xintercept = as.numeric(interest), color = col, linetype = linet) # Add vertical line at point of interest

  k <- as.factor(k) # Turn numerical list value to categorical values for legend

  for (i in 1:length(predstorage)){ # Create loop to iterate through list for changepoint predictions on plot

    predsegx <- predstorage[[i]][[xname]] # Pull w from list

    guess <- predstorage[[i]]$predseg # Pull predictions from list

    predsegplot <- predsegplot + geom_line(aes_string(x = `predsegx`, y = guess, color = k[i]), size = 1.2, alpha = 0.6) + # Plot predictions with different colors for each line; size changed for thickness and alpha for opaqueness
      scale_color_discrete(name = "Max Changepoints") + # Change name of legend
      ggtitle(paste(paste(changepoints, collapse = ", "), "Automatically Detected Changepoints from", (yname), "Using", z)) # Add title with predicted changepoints
  }

  predsegplot <- ggplotly(predsegplot) # Turn ggplot into interactive plot using plotly

  return(predsegplot) # Return combined segmented plot
}
