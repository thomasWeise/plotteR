% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/batchPlotList.R
\name{batchPlot.list}
\alias{batchPlot.list}
\title{Plot a Set of Curves from a List of Data}
\usage{
batchPlot.list(data, xfun = function(x) x$x, yfun = function(x) x$y,
  ffun = function(x) x$f, plotXY = TRUE, widthXY = 0.5,
  plotXF = TRUE, widthXF = 1.5, names = NULL,
  colors = colors.distinct(length(data)), xlab = "", ylab = "",
  legend = NULL, x.min.lower = NA, x.min.upper = NA,
  x.max.lower = NA, x.max.upper = NA, y.min.lower = NA,
  y.min.upper = NA, y.max.lower = NA, y.max.upper = NA,
  x.add = NULL, XYType = "p", XFType = "l", ...)
}
\arguments{
\item{data}{the data object, could be a list of lists or anything}

\item{xfun}{a function which receives an element from the \code{data} list
and extracts a vector of \code{x}-coordinates from it}

\item{yfun}{a function which receives an element from the \code{data} list
and extracts a vector of \code{y}-coordinates from it to be plotted as
points, or \code{NULL} if no points should be plotted (see \code{plotXY})}

\item{ffun}{a function which receives an element from the \code{data} list
and extracts a unary function from it to be plotted over the extracted
\code{x} coordinates, or \code{NULL} if no points should be plotted (see
\code{plotXF})}

\item{plotXY}{should the \code{x-y} points be plotted (if \code{yfun} is not
\code{NULL})}

\item{widthXY}{the line width for points to be plotted (only considered if
\code{plotXY} is \code{TRUE} and \code{yfun} is not \code{NULL})}

\item{plotXF}{should the \code{x-y} lines be plotted (if \code{ffun} is not
\code{NULL})}

\item{widthXF}{the line width for lines to be plotted (only considered if
\code{plotXF} is \code{TRUE} and \code{ffun} is not \code{NULL})}

\item{names}{the names of the lines to be printed in the legend, or
\code{NULL} if no legend should be plotted}

\item{colors}{the colors to be used for the plot}

\item{xlab}{the label for the x-axis}

\item{ylab}{the label for the y-axis}

\item{legend}{a list of additional parameters to be passed to
\code{\link[graphics]{legend}}, or \code{NULL} to use the default
parameters}

\item{x.min.lower}{a lower bound for the automatically computed \code{x}
coordinate minimum}

\item{x.min.upper}{an upper bound for the automatically computed \code{x}
coordinate minimum}

\item{x.max.lower}{a lower bound for the automatically computed \code{x}
coordinate maximum}

\item{x.max.upper}{an upper bound for the automatically computed \code{x}
coordinate maximum}

\item{y.min.lower}{a lower bound for the automatically computed \code{y}
coordinate minimum}

\item{y.min.upper}{an upper bound for the automatically computed \code{y}
coordinate minimum}

\item{y.max.lower}{a lower bound for the automatically computed \code{y}
coordinate maximum}

\item{y.max.upper}{an upper bound for the automatically computed \code{y}
coordinate maximum}

\item{x.add}{some additional \code{x} coordinates at which the function
should be evaluated, or \code{TRUE} if the \code{x} coordinate minimum and
maximum over all data sets should be added as evaluation points}

\item{XYType}{the type in which the XY data should be plotted, by default
\code{"p"}, i.e., as points}

\item{XFType}{the type in which the XF data should be plotted, by default
\code{"l"}, i.e., as lines}

\item{...}{Arguments passed on to \code{graphics::plot}
\describe{
  \item{}{}
}}
}
\description{
A simple utility method for visualizing a list of data.
}
\examples{
library(plotteR)

# set a random seed for replicability
set.seed(1367);

# make an example
make.example <- function(f) {
  n <- as.integer(round(runif(n=1, min=10, max=200)));
  x <- sort(runif(n=n, min=0, max=3)); # generate x data
  y <- rnorm(n=n, mean=f(x), s=0.1);  # noisy y
  x <- rnorm(n=n, mean=x, s=0.1); # noisy x
  return(list(x=x, y=y, f=f));
}

# the three base functions
f <- c(function(x) 1 - 0.2*x + 0.75*x*x - 0.3*x*x*x,
       function(x) 0.1 * exp(3 - x),
       function(x) 1.2 + 0.7*sin(2*x));

# create the three example data sets
examples <- lapply(X=f, FUN=make.example);

# plot the original data
batchPlot.list(examples,
               names=c("f1", "f2", "f3"),
               main="Original Data and Function Values for x",
               legend=list(x="bottom", horiz=TRUE));
library(plotteR)

# set a random seed for replicability
set.seed(1367);

# make an example
make.example <- function(f) {
  suppressWarnings({
    repeat {
      n <- as.integer(round(runif(n=1, min=10, max=20)));
      x <- sort(runif(n=n, min=-5, max=5)); # generate x data
      y <- rnorm(n=n, mean=f(x), s=0.1);  # noisy y
      x <- rnorm(n=n, mean=x, s=0.1); # noisy x
      fi <- is.finite(x) & is.finite(y);
      x <- x[fi];
      if(length(x) > 4L) {
        y <- y[fi];
        return(list(x=x, y=y, f=f));
      }
    }
  });
}

# the three base functions
f <- c(function(x) 1 - 0.2*x + 0.75*x*x - 0.3*x*x*x,
       log,   # this function becomes non-finite for x <= 0
       asin); # non-finite for any x outside of [-1, 1]

# create the three example data sets
examples <- lapply(X=f, FUN=make.example);

old <- par(mfcol=c(2, 1));

# plot the original data, which only covers part of the x.axis
batchPlot.list(examples,
               names=c("f1", "f2", "f3"),
               main="Original Data and Function Values for x",
               legend=list(x="left"),
               y.min.lower = -3,
               y.max.upper= 3);
abline(v=-1, col="gray");abline(v=0, col="gray");abline(v=1, col="gray");


# extending the function lines towards their smallest and largest finite
# points
batchPlot.list(examples,
               names=c("f1", "f2", "f3"),
               main="Functions Extented by Finite Point Search",
               legend=list(x="left"),
               y.min.lower = -3,
               y.max.upper= 3,
               x.add = TRUE);
abline(v=-1, col="gray");abline(v=0, col="gray");abline(v=1, col="gray");

par(old);
}
