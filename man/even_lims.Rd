% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/even_lims.R
\name{even_lims}
\alias{even_lims}
\title{Set Even Limits Based on Data Values}
\usage{
even_lims(vec)
}
\arguments{
\item{vec}{A vector, typically a row or column representing a dimension to plot}
}
\value{
A length 2 vector consisting of the negative and positive of the value of @param vec
with the greatest absolute value,
respectively rounded down and up.
}
\description{
This function takes a numeric vector and returns a range of the negative and positive of the element with the highest absolute
value. The lower bound is rounded down to the next integer, the upper bound rounded up.
It is intended to set even limits on plots when called on the breaks or data values corresponding to an aesthetic,
and usefully always returns an interval containing 0.
}
\examples{
# Create four-quadrant plot
library(ggplot2)

}
