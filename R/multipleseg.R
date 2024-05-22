#' multipleseg
#'
#' multipleseg plots user prespecified changepoint predictions over the original data and produces an interactive plotly graph.
#'
#' @param df A dataframe that will be processed.
#' @param d Numeric or date vector that will become the x-axis for the final plot.
#' @param y Numeric response vector that is the same length as d.
#' @param n Numeric vector containing the desired changepoints to be predicted.
#' @param interest Vector containing any points of interest; these points of interest should be on the same scale as d.
#' @param col Color of line to indicate points of interest (default set to "red").
#' @param linet Type of vertical line to indicate points of interest (default set to "dashed").
#' @param date Boolean indicating if the points of interest are dates (default set to FALSE).
#' @return Returns a plotly interactive graph that shows the projected changepoint predictions.
#' @export
#' @import tidyverse
#' @import segmented
#' @import plotly
#' @import zoo

multipleseg <- function(df, d, y, n, interest = c(), col = "red", linet = "dashed", date = FALSE){

  if (date == TRUE){
    interest <- as.Date(interest, format = "%m/%d/%Y") # Convert points of interest into a date if specified to be a date
  }

  x <- as.numeric(d) # Convert variable to numeric

  xname <- gsub(".*\\$", "", deparse(substitute(d))) # Extract variable name from input

  yname <- gsub(".*\\$", "", deparse(substitute(y)))

  n <- sort(n) # Sort number of changepoints in ascending order

  segfit <- lm(y ~ 1 + x, data = df) # Run linear model using selected variables with an intercept

  storage <- list() # Create empty list to store dataframes

  for (i in 1:length(n)){

    smoothseg <- segmented(segfit, seg.Z = ~ x, npsi = n[[i]]) # Use segmented function to find n changepoints

    seg.predict <- cbind(na.omit(df), predict(smoothseg)) # Combine non-missing values with predictions

    seg.predict <- seg.predict %>% rename(smoothseg = "predict(smoothseg)") # Rename predictions for easier reference

    storage[[i]] <- seg.predict # Store dataframes with predictions in list
  }

  segplot <- ggplot(storage[[1]], aes(x = d, y = y)) + # Initialize plot with original observations
    labs(x = xname, y = yname) +
    geom_point(size = 0.5) +
    geom_vline(xintercept = as.numeric(interest), color = col, linetype = linet) # Add vertical line at point of interest

  n <- as.factor(n) # Turn numerical list value to categorical values for legend

  for (i in 1:length(storage)){ # Create loop to iterate through list for changepoint predictions on plot

    segx <- storage[[i]][[xname]] # Pull d from list

    guess <- storage[[i]]$smoothseg # Pull predictions from list

    segplot <- segplot + geom_line(aes_string(x = `segx`, y = guess, color = n[i]), size = 1.2, alpha = 0.6) + # Plot predictions with different colors for each line; size changed for thickness and alpha for opaqueness
      scale_color_discrete(name = "Changepoints") + # Change name of legend
      ggtitle(paste((yname), "Changepoint Predictions with", paste(n, collapse = ", "), "Changepoints")) # Add title
  }

  segplot <- ggplotly(segplot) # Turn ggplot into interactive plot using plotly

  return(segplot) # Return interactive plot
}
