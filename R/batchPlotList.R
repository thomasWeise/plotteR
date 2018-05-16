# create a set of reasonable x coordinates
.make.x.coords <- function(x) {
  n <- length(x);
  if(n <= 0L) { return(NULL); }

  if(n > 1L) {
    # there are at least two points, so we need to sort
    x <- sort.int(x);

    if(n < 40L) {
      # if there are not enough points, just add some uniformly distributed points
      x <- sort.int(x, partial=c(1L, n));
      x <- sort.int(c(x, (x[1L] + (((1L:35L)/36L) * (x[n] - x[1L])))));
      n <- length(x);
    }

    # there are now enough points
    # but there could be big gaps between the first and second OR the last and
    # second-to-last point

    add <- NULL; # the points to be added, if necessary
    ap  <- max(10L, n %/% 3L); # the number of points to be added, if necessary

    # compute the x range
    ran.w <- x[n] - x[1L];

    # check if there is too big of a gap at the beginning
    x.d <- (x[2L] - x[1L]);
    if( (x.d / ran.w) > 0.2) {
      # there is much space between the first and the second point
      # so we insert some uniformly distributed points in between
      add <- (x[1L] + ((seq_len(ap) / (ap + 1L)) * x.d));
    }

    # check if there is too big of a gap at the end
    x.d <- (x[n] - x[n- 1L]);
    if( (x.d / ran.w) > 0.2) {
      # there is much space between the first and the second point
      # so we insert some uniformly distributed points in between
      add <- c(add, (x[n - 1L] + ((seq_len(ap) / (ap + 1L)) * x.d)));
    }

    if(!(is.null(add))) {
      # add the points
      x <- sort.int(c(x, add));
    }
  }

  # return the unique, finite points in x
  return(unique(x[is.finite(x)]));
}

# compute the function value of f for all values of y
#' @importFrom utilizeR find.finite
.compute.f <- function(x, f) {

  if(is.null(x)) { return(NULL); }
  n <- length(x);
  if(n <= 0L) { return(NULL); }

  # sort the first and last two points
  x <- sort.int(x, partial=c(1L, 2L, n-1L, n));

  # make start finite
  r <- find.finite(x[1L], x[2L], f);
  if(!(is.finite(r[2L]))) {
    r <- find.finite(x[1L], x[n - 1L], f);
    if(!(is.finite(r[2L]))) {
      r <- find.finite(x[1L], x[n], f);
    }
  }
  a     <- r[1L];
  x[1L] <- a;

  # make end finite
  r <- find.finite(x[n], x[n-1L], f);
  if(!(is.finite(r[2L]))) {
    r <- find.finite(x[n], x[2L], f);
    if(!(is.finite(r[2L]))) {
      r <- find.finite(x[n], x[1L], f);
    }
  }
  b    <- r[1L];
  x[n] <- b;

  # get a reasonable set of coordinates
  x <- .make.x.coords(x[ (x >= a) & (x <= b) ]);

  # ok, continue
  if(is.null(x)) { return(NULL); }
  n <- length(x);
  if(n <= 0L) { return(NULL); }

  # ok, we now have a reasonable set of points
  y <- f(x);
  stopifnot(identical(n, length(y)));

  if(all(is.finite(y))) {
    # ok, we could compute the function for all x-coordinates
    # so we can return the result
    return(list(x=x, y=y));
  }

  # no, at least some coordinates are not finite
  # so we attempt to fix them
  for(i in seq_len(n)) {
    xx <- x[i];
    yy <- y[i];
    if(!(is.finite(yy))) {
      # ok, yy is not finite

      if(i > 1L) { # get a finite point from the left
        r1 <- find.finite(xx, x[i - 1L], f);
      } else { # no previous point
        r1 <- c(xx, yy);
      }

      if(i < n) { # get a finite point from the right
        r2 <- find.finite(xx, x[i + 1L], f);
      } else { # no next point
        r2 <- r1;
      }

      if(is.finite(r1[2L])) {
# ok, there is a finite point from the left
        if(is.finite(r2[2L]) && (abs(r1[1L] - xx) > abs(r2[1L] - xx))) {
# but there is a closer finite on the right
          x[i] <- r2[1L];
          y[i] <- r2[2L];
        } else {
# and there is no closer finite point on the right
          x[i] <- r1[1L];
          y[i] <- r1[2L];
        }
      } else {
# there is no finite point on the left, but a finite point on the right
        if(is.finite(r2[2L])) {
          x[i] <- r2[1L];
          y[i] <- r2[2L];
        }
      }
    }
  }

  # choose only the finite points in the list
  fin <- is.finite(y);
  x <- x[fin];
  if(length(x) <= 0L) { return(NULL); }
  y <- y[fin];
  return(list(x=x, y=y));
}

#' @title Plot a Set of Curves from a List of Data
#' @description A simple utility method for visualizing a list of data.
#' @param data the data object, could be a list of lists or anything
#' @param xfun a function which receives an element from the \code{data} list
#'   and extracts a vector of \code{x}-coordinates from it
#' @param yfun a function which receives an element from the \code{data} list
#'   and extracts a vector of \code{y}-coordinates from it to be plotted as
#'   points, or \code{NULL} if no points should be plotted (see \code{plotXY})
#' @param ffun a function which receives an element from the \code{data} list
#'   and extracts a unary function from it to be plotted over the extracted
#'   \code{x} coordinates, or \code{NULL} if no points should be plotted (see
#'   \code{plotXF})
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
#' @param x.min.lower a lower bound for the automatically computed \code{x}
#'   coordinate minimum
#' @param x.min.upper an upper bound for the automatically computed \code{x}
#'   coordinate minimum
#' @param x.max.lower a lower bound for the automatically computed \code{x}
#'   coordinate maximum
#' @param x.max.upper an upper bound for the automatically computed \code{x}
#'   coordinate maximum
#' @param y.min.lower a lower bound for the automatically computed \code{y}
#'   coordinate minimum
#' @param y.min.upper an upper bound for the automatically computed \code{y}
#'   coordinate minimum
#' @param y.max.lower a lower bound for the automatically computed \code{y}
#'   coordinate maximum
#' @param y.max.upper an upper bound for the automatically computed \code{y}
#'   coordinate maximum
#' @param x.add some additional \code{x} coordinates at which the function
#'   should be evaluated, or \code{TRUE} if the \code{x} coordinate minimum and
#'   maximum over all data sets should be added as evaluation points
#' @inheritDotParams graphics::plot -x -y
#' @include distinctColors.R
#' @export batchPlot.list
#' @importFrom graphics plot
#' @example examples/batchPlotList.R
#' @example examples/batchPlotList2.R
batchPlot.list <- function(data,
                           xfun=function(x) x$x,
                           yfun=function(x) x$y,
                           ffun=function(x) x$f,
                           plotXY=TRUE,
                           widthXY=0.5,
                           plotXF=TRUE,
                           widthXF=1.5,
                           names=NULL,
                           colors=colors.distinct(length(data)),
                           xlab="",
                           ylab="",
                           legend=NULL,
                           x.min.lower=NA,
                           x.min.upper=NA,
                           x.max.lower=NA,
                           x.max.upper=NA,
                           y.min.lower=NA,
                           y.min.upper=NA,
                           y.max.lower=NA,
                           y.max.upper=NA,
                           x.add=NULL,
                           ...) {
  .batchPlot.list(data=data, xfun=xfun, yfun=yfun,
                  ffun=ffun, plotXY=plotXY, widthXY=widthXY,
                  plotXF=plotXF, widthXF=widthXF, names=names,
                  colors=colors, legendColors=colors,
                  legend=legend,
                  xlab=xlab, ylab=ylab,
                  x.min.lower=x.min.lower,
                  x.min.upper=x.min.upper,
                  x.max.lower=x.max.lower,
                  x.max.upper=x.max.upper,
                  y.min.lower=y.min.lower,
                  y.min.upper=y.min.upper,
                  y.max.lower=y.max.lower,
                  y.max.upper=y.max.upper,
                  x.add=x.add, ...);
}

# check a function's arguments
.check.f <- function(f) {
  if(is.primitive(f)) {
    a <- formals(args(f));
  } else {
    a <- formals(f);
  }
  stopifnot(identical(names(a), c("x")));
}

# the internal implementation which is also used by data groups
#' @importFrom graphics plot points lines legend
.batchPlot.list <- function(data,
                            xfun=function(x) x$x,
                            yfun=function(x) x$y,
                            ffun=function(x) x$f,
                            plotXY=TRUE,
                            widthXY=0.5,
                            plotXF=TRUE,
                            widthXF=1.5,
                            names=NULL,
                            colors=colors.distinct(length(data)),
                            legendColors=colors,
                            legend=NULL,
                            xlab="",
                            ylab="",
                            x.min.lower=NA,
                            x.min.upper=NA,
                            x.max.lower=NA,
                            x.max.upper=NA,
                            y.min.lower=NA,
                            y.min.upper=NA,
                            y.max.lower=NA,
                            y.max.upper=NA,
                            x.add=NULL,
                            ...) {
# basic sanity tests
  stopifnot( ((plotXY && (widthXY > 0)) || (plotXF && (widthXF > 0))) &&
              (widthXY >= 0) && (widthXF >= 0) &&
              (!(is.null(colors))) &&
              identical(length(data), length(colors)));

  .check.f(xfun); # check x function

  # check y function
  if(plotXY) {
    if(is.null(yfun)) { plotXY <- FALSE; }
    else { .check.f(yfun); }
  } else { yfun <- NULL; }

  # check f function
  if(plotXF) {
    if(is.null(ffun)) { plotXF <- FALSE; }
    else { .check.f(ffun); }
  } else { ffun <- NULL; }

  # extract the x-coordinates
  x.all <- lapply(X=data, FUN=xfun);

  # compute x range
  x.min <- +Inf;
  x.max <- -Inf;
  for(x.cur in x.all) {
    x.cur <- x.cur[is.finite(x.cur)];
    if(length(x.cur) > 0L) {
      x.min <- min(x.min, x.cur);
      x.max <- max(x.max, x.cur);
    }
  }

  # extend the axes by the additional x coordinates
  if((!(is.null(x.add))) && is.numeric(x.add)) {
    x.min <- min(x.min, x.add);
    x.max <- max(x.max, x.add);
  }

  # apply the boundaries to the x coordinate range
  if(!(is.na(x.min.lower))) { x.min <- max(x.min.lower, x.min); }
  if(!(is.na(x.min.upper))) { x.min <- min(x.min.upper, x.min); }
  if(!(is.na(x.max.lower))) { x.max <- max(x.max.lower, x.max); }
  if(!(is.na(x.max.upper))) { x.max <- min(x.max.upper, x.max); }

  # if x.add is TRUE, we also add the minimum and maximum x-coordinate
  if((!(is.null(x.add))) && is.logical(x.add) && isTRUE(x.add)) {
    x.add <- c(x.min, x.max);
  }

  # obtain the y data
  suppressWarnings({
  data <- lapply(X=seq_along(data),
                 FUN=function(i) {
                   # get the x coordinates
                   x <- x.all[[i]];
                   d <- data[[i]];

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

                   xs <- NULL;
                   f  <- NULL;

                   # if wanted, get the xs/f pairs
                   if(plotXF && (!(is.null(ffun)))) {
                     # compute the function results
                     res <- .compute.f(c(x, x.add), ffun(d));

                     if(!(is.null(res))) {
                       xs <- res$x;
                       f  <- res$y;
                       if(identical(xs, x)) { xs <- x; }
                     }
                   }

                   # build the list with all the data
                   return(list(x=x, y=y, xs=xs, f=f));
                 })
  });
  # check that the resulting list has the same length as the colors list
  stopifnot(identical(length(data), length(colors)));

  x.all <- NULL; # release no longer neded resources
  x.add <- NULL; # release no longer neded resources

  # get the y coordinate ranges
  y.min <- +Inf;
  y.max <- -Inf;
  for(d in data) {
    y <- d$y;
    # get min and max y from the y point coordinates, if given
    if(!(is.null(y))) {
      y <- y[is.finite(y)];
      if(length(y) > 0L) {
        y.min <- min(y.min, y);
        y.max <- max(y.max, y);
      }
    }

    y <- d$f;
    # if given, get the min/max for the y of the lines
    if(!(is.null(y))) {
      y <- y[is.finite(y)];
      if(length(y) > 0L) {
        y.min <- min(y.min, y);
        y.max <- max(y.max, y);
      }
    }
  }

  # apply the boundaries to the y coordinate range
  if(!(is.na(y.min.lower))) { y.min <- max(y.min.lower, y.min); }
  if(!(is.na(y.min.upper))) { y.min <- min(y.min.upper, y.min); }
  if(!(is.na(y.max.lower))) { y.max <- max(y.max.lower, y.max); }
  if(!(is.na(y.max.upper))) { y.max <- min(y.max.upper, y.max); }

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
