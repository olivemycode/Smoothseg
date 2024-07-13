#' qreg
#'
#' qreg creates a quantile regression model using specified predictor and response variables.
#'
#' @param df A dataframe that will be processed.
#' @param x The predictor variable for quantile regression; this can be a vector of strings.
#' @param y The response variable as a string for quantile regression.
#' @param tau The quantile to be estimate (set to 0.5 by default)
#' @param intercept Determines if there is an intercept in the model (set to TRUE by default).
#' @return Returns a fitted model.
#' @export
#' @import forecast
#' @import stats
#' @import dplyr
#' @import quantreg



qreg <- function(df, x, y, tau = 0.5, intercept = TRUE) {

  if (intercept) {
    f <- as.formula(paste(response, "~", paste(predictors, collapse = "+")))
  }

  else {
    f <- as.formula(paste(response, "~", paste(predictors, collapse = "+"), "-1"))
  }

  model <- rq(f, data = df, tau = tau) # Create quantile regression model

  return(model)
}
