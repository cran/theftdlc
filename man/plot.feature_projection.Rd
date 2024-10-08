% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot.feature_projection}
\alias{plot.feature_projection}
\title{Produce a plot for a feature_projection object}
\usage{
\method{plot}{feature_projection}(x, show_covariance = TRUE, ...)
}
\arguments{
\item{x}{\code{feature_projection} object containing the two-dimensional embedding calculated by \code{project}}

\item{show_covariance}{\code{Boolean} specifying whether covariance ellipses should be shown on the plot. Defaults to \code{TRUE}}

\item{...}{Arguments to be passed to methods}
}
\value{
object of class \code{ggplot} that contains the graphic
}
\description{
Produce a plot for a feature_projection object
}
\author{
Trent Henderson
}
