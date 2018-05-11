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
                   if(plotXY || (!(is.null(yfun)))) {
                     y <- yfun(d);
                     stopifnot(identical(length(y), length(x)));
                   } else {
                     y <- NULL;
                   }

                   # if wanted, get the xs/f pairs
                   if(plotXF && (!(is.null(ffun)))) {
                     xs <- sort(x);
                     if(identical(xs, x)) { xs <- x; }
                     f <- ffun(d, xs);
                     stopifnot(identical(length(f), length(xs)));
                   } else {
                     xs <- NULL;
                     f  <- NULL;
                   }
                   list(x=x, y=y, xs=xs, f=f)
                 });
  stopifnot(identical(length(data), length(colors)));

  # get the x and y coordinate ranges
  for(d in data) {
    x <- d$x;
    x.min <- min(x.min, x);
    x.max <- max(x.max, x);

    y <- d$y;
    if(!(is.null(y))) {
      y.min <- min(y.min, y);
      y.max <- max(y.max, y);
    }

    y <- d$f;
    if(!(is.null(y))) {
      y.min <- min(y.min, y);
      y.max <- max(y.max, y);
    }
  }

  # create the dummy plot
  plot(x=c(x.min, x.max), y=c(y.min, y.max), type="n", xlab=xlab, ylab=ylab, ...);

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
    stopifnot(identical(length(names), length(legendColors)));
    if(missing(legend) || is.null(legend)) {
      legend <- list();
    }
    legend$text.col = legendColors;
    legend$col = legendColors;

    legend$legend = names;
    if(is.null(legend$x)) {
      legend$x <- "topright";
    }
    if(is.null(legend$lwd)) {
      if(plotXF) { legend$lwd <- widthXF; }
      else { if(plotXY) { legend$lwd <- widthXY; } }
    }

    # add a legend to the plot
    do.call(graphics::legend, legend);
  }
}
