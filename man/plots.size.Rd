% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotSize.R
\name{plots.size}
\alias{plots.size}
\title{Suggest Proper Width and Height for a Graphic with a Certain Number of
  Diagram Rows and Columns}
\usage{
plots.size(rows = 1L, columns = 1L, single.minWidth.in = 2.2,
  single.minHeight.in = max(1, single.minWidth.in/.goldenRatio))
}
\arguments{
\item{rows}{the rows}

\item{columns}{the columns}

\item{single.minWidth.in}{the minimum width of a diagram in inch}

\item{single.minHeight.in}{the minimum height of a diagram in inch}
}
\value{
a vector \code{c(width, height)} with appropriate width and height in
  inch of the graphic
}
\description{
Given a number of \code{rows} and \code{columns}, make a
  suggestion about the \code{c(width, height)} in inch that a graphic should
  have to accommodate the figure.
}
