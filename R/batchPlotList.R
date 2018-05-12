#' @title Plot a Set of Curves from a List of Data
#' @description A simple utility method for visualizing a list of data.
#' @param data the data object, could be a list of lists or anything
#' @param xfun a function which receives an element from the \code{data} list
#'   and extracts a vector of \code{x}-coordinates from it
#' @param yfun a function which receives an element from the \code{data} list
#'   and extracts a vector of \code{y}-coordinates from it to be plotted as
#'   points, or \code{NULL} if no points should be plotted (see \code{plotXY})
#' @param ffun a function which receives an element from the \code{data} list
#'   and the corresponding extracted \code{x}-coordinates and extracts or
#'   computes a vector of \code{y}-coordinates from it to be plotted as line, or
#'   \code{NULL} if no points should be plotted (see \code{plotXF})
#' @param plotXY should the \code{x-y} points be plotted (if \code{yfun} is not
#'   \code{NULL})
#' @param widthXY the line width for points to be plotted (only considered if
#'   \code{plotXY} is \code{TRUE} and \code{yfun} is not \code{NULL})
#' @param plotXF should the \code{x-y} lines be plotted (if \code{ffun} is not
#'   \code{NULL})
#' @param widthXF the line width for lines to be plotted (only considered if
#'   \code{plotXF} is \code{TRUE} and \code{ffun} is not \code{NULL})
#' @param names the names of the lines to be printed in the legend, or
#'   \code{NULL} if no legend should be plotted
#' @param colors the colors to be used for the plot
#' @param xlab the label for the x-axis
#' @param ylab the label for the y-axis
#' @param legend a list of additional parameters to be passed to
#'   \code{\link[graphics]{legend}}, or \code{NULL} to use the default
#'   parameters
#' @inheritDotParams graphics::plot -x -y
#' @include distinctColors.R
#' @export batchPlot.list
#' @importFrom graphics plot
#' @example examples/batchPlotList.R
batchPlot.list <- function(data,
                           xfun=function(d) d$x,
                           yfun=function(d) d$y,
                           ffun=function(d, x) yfun(d),
                           plotXY=TRUE,
                           widthXY=0.5,
                           plotXF=TRUE,
                           widthXF=1.5,
                           names=NULL,
                           colors=colors.distinct(length(data)),
                           xlab="",
                           ylab="",
                           legend=NULL,
                           ...) {
  .batchPlot.list(data=data, xfun=xfun, yfun=yfun,
                  ffun=ffun, plotXY=plotXY, widthXY=widthXY,
                  plotXF=plotXF, widthXF=widthXF, names=names,
                  colors=colors, legendColors=colors,
                  legend=legend,
                  xlab=xlab, ylab=ylab, ...);
}



# the internal implementation which is also used by data groups
#' @importFrom graphics plot points lines legend
.batchPlot.list <- function(data,
                            xfun=function(d) d$x,
                            yfun=function(d) d$y,
                            ffun=function(d, x) yfun(d),
                            plotXY=TRUE,
                            widthXY=0.5,
                            plotXF=TRUE,
                            widthXF=1.5,
                            names=NULL,
                            colors=colors.distinct(length(data)),
                            legendColors=colors,
                            legend=NULL,
                            xlab="",
                            ylab="", ...) {

  stopifnot( ((plotXY && (widthXY > 0)) || (plotXF && (widthXF > 0))) &&
              (widthXY >= 0) && (widthXF >= 0) &&
              (!(is.null(colors))) &&
              (identical(length(data), length(colors))));

  x.min <- +Inf;
  y.min <- +Inf;
  x.max <- -Inf;
  y.max <- -Inf;

  # obtain the data
  data <- lapply(X=data,
                 FUN=function(d) {

                   # get the x coordinates
                   x <- xfun(d);

                   # if wanted, get the associated y coordinates
                   if(plotXY && (!(is.null(yfun)))) {
                     # extract the y values
                     y <- yfun(d);
                     # and make sure they have the right length
                     stopifnot(identical(length(y), length(x)));
                   } else {
                     # no y values
                     y <- NULL;
                   }

                   # if wanted, get the xs/f pairs
                   if(plotXF && (!(is.null(ffun)))) {
                     # sort x, so that no lines go back and forth
                     xs <- sort(x);
                     # use original x if the same as xs
                     if(identical(xs, x)) { xs <- x; }
                     # apply the function
                     f <- ffun(d, xs);
                     # stop if it failed
                     stopifnot(identical(length(f), length(xs)));
                   } else {
                     # no function, no xs and f
                     xs <- NULL;
                     f  <- NULL;
                   }

                   # build the list with all the data
                   list(x=x, y=y, xs=xs, f=f)
                 });
  # check that the resulting list has the same length as the colors list
  stopifnot(identical(length(data), length(colors)));

  # get the x and y coordinate ranges
  for(d in data) {
    x <- d$x;
    # we must have x coordinates
    x.min <- min(x.min, x);
    x.max <- max(x.max, x);

    y <- d$y;
    # get min and max y from the y point coordinates, if given
    if(!(is.null(y))) {
      y.min <- min(y.min, y);
      y.max <- max(y.max, y);
    }

    y <- d$f;
    # if given, get the min/max for the y of the lines
    if(!(is.null(y))) {
      y.min <- min(y.min, y);
      y.max <- max(y.max, y);
    }
  }

  # create the dummy plot of exactly the right size
  plot(x=c(x.min, x.max),
       y=c(y.min, y.max),
       type="n", xlab=xlab, ylab=ylab, ...);

  # actually paint the plot
  for(index in seq_along(data)) {
    color <- colors[index];
    d     <- data[[index]];

    # paint the points, if wanted
    x <- d$x;
    y <- d$y;
    if(plotXY && (widthXY > 0) && (!(is.null(y)))) {
      points(x=x, y=y, col=color, lwd=widthXY);
    }

    # paint the lines, if wanted
    x <- d$xs;
    y <- d$f;
    if(plotXF && (widthXF > 0) && (!(is.null(y) || is.null(x)))) {
      lines(x=x, y=y, col=color, lwd=widthXF);
    }
  }

  # should we have a legend
  if(!(is.null(names))) {
    # then we must have as many names as legend colors
    stopifnot(identical(length(names), length(legendColors)));

    # if no legend data is given, create empty list
    if(missing(legend) || is.null(legend)) {
      legend <- list();
    }

    # set the colors for the text and lines in the legend
    legend$text.col <- legendColors;
    legend$col <- legendColors;

    # store the legend names
    legend$legend <- names;
    if(is.null(legend$x)) {
      legend$x <- "topright";
    }

    # if no line width is given, use the provided widths
    if(is.null(legend$lwd)) {
      if(plotXF) { legend$lwd <- widthXF; }
      else { if(plotXY) { legend$lwd <- widthXY; } }
    }

    # add a legend to the plot
    do.call(graphics::legend, legend);
  }
}
