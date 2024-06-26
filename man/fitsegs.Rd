% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitsegs.R
\name{fitsegs}
\alias{fitsegs}
\title{fitsegs}
\usage{
fitsegs(
  df,
  w,
  y,
  n,
  interest = c(),
  col = "red",
  linet = "dashed",
  date = FALSE,
  ar = FALSE,
  p = 1,
  d = 0,
  q = 0
)
}
\arguments{
\item{df}{A dataframe that will be processed.}

\item{w}{Numeric or date vector that will become the x-axis for the final plot.}

\item{y}{Numeric response vector that is the same length as d.}

\item{n}{Numeric vector containing the desired changepoints to be predicted.}

\item{interest}{Vector containing any points of interest; these points of interest should be on the same scale as d.}

\item{col}{Color of line to indicate points of interest (default set to "red").}

\item{linet}{Type of vertical line to indicate points of interest (default set to "dashed").}

\item{date}{Boolean indicating if the points of interest are dates (default set to FALSE).}

\item{ar}{Boolean indicating if the model needed is an AR model.}

\item{p}{AR order that only applies if ar = TRUE (default set to 1).}

\item{d}{Degree of differencing if ar = TRUE (default set to 0).}

\item{q}{MA order if ar = TRUE (default set to 0).}
}
\value{
Returns a plotly interactive graph that shows the projected changepoint predictions.
}
\description{
fitsegs plots user prespecified changepoint predictions over the original data and produces an interactive plotly graph.
When working with timeseries, an AR(1) model is returned by default.
}
