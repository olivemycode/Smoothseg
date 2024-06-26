% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rescalex.R
\name{rescalex}
\alias{rescalex}
\title{rescalex}
\usage{
rescalex(df, x, varname = "scalex")
}
\arguments{
\item{df}{A dataframe that will be processed.}

\item{x}{Numeric vector that will be rescaled to be from 0 to 1.}

\item{varname}{The variable name for the rescaled variable (default is "scalex")}
}
\value{
Returns a dataframe that contains an additional column with the rescaled x variable.
}
\description{
rescalex rescales variables to be on a scale from 0 to 1. This is useful for examining date variables and when events occur
within the observation period.
}
