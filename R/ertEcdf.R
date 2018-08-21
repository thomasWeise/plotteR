#' @title Plot ERT-ECDF Functions
#' @description Plot the Empirical Cumulative Distribution Functions (ECDFs) for
#'   a couple of different datasets which, in turn, are obtained as Expected Running Times from experiments.
#' @param x the data, maybe a list of [lists of matrices or a list of list of
#'   vectors/lists], where each element has at least the \code{goal.dim} and
#'   \code{time.dim} dimension.
#' @param goal the goal value, i.e., the value which must be reached to be
#'   counted as success
#' @param goal.dim the dimension where where the goal values can be found
#' @param time.dim the dimension where the time values can be found
#' @param comparator the comparator, usually \code{`<=`} or \code{`>=`}
#' @param time.type the type function the time dimension, should be
#'   \code{as.integer} or \code{as.numeric}
#' @param time.min the minimum time value to be used for the diagram, or
#'   \code{NA} to use the smallest time value in any run
#' @param time.max the maximum time value to be used, or \code{NA} to pick the
#'   maximum time value of any run
#' @param goal.min the minimum goal value
#' @param goal.max the maximum goal value
#' @param extract.run a function which can be used to extract the single runs
#'   from the run sets, e.g., \code{identity}
#' @param extract.runs a function which can be used to extract the run sets from
#'   \code{x}, e.g., \code{identity}
#' @param lineTypeFun the line type function, a function returning the line
#'   types to use for a provided number of inputs
#' @param colorFun the colors function, a function returning the color list to
#'   use for a provided number of inputs
#' @param legend the legend names
#' @param legend.pos the legend position (optional, default: topleft)
#' @param legend.cex the character sizing for the legend, optional, default
#'   \code{NA}
#' @param goal.markers markers for the goal values
#' @param time.markers markers for the time values
#' @return a \code{list(x=c(time.min, time.max), y=c(goal.min, goal.max))} of
#'   the minimum and maximum time and goal values encountered
#' @inheritDotParams graphics::plot
#' @export plot.func.ertEcdf
#' @include ertEcdfFun.R
#' @include distinctLineTypes.R
#' @include distinctColors.R
#' @importFrom graphics plot abline legend lines
#' @example examples/ecdf.R
plot.func.ertEcdf <- function(x,
                             goal=0,
                             goal.dim=2L,
                             time.dim=1L,
                             time.type=as.integer,
                             time.min=time.type(1L),
                             time.max=time.type(NA_real_),
                             goal.min=0,
                             goal.max=1,
                             extract.runs=identity,
                             comparator=`<=`,
                             extract.run=identity,
                             lineTypeFun=lineTypes.distinct,
                             colorFun=colors.distinct,
                             goal.markers=c(0.2,0.4,0.6,0.8),
                             time.markers=NULL,
                             legend=NULL,
                             legend.pos="topleft",
                             legend.cex=NA,
                             ...) {

  # compute the data
  x <- lapply(X=x,
              FUN=func.ertEcdf,
              goal=goal,
              goal.dim=goal.dim,
              time.dim=time.dim,
              comparator=comparator,
              time.type=time.type,
              goal.type=as.numeric,
              extract.run=extract.run,
              extract.runs=extract.runs);

  # re-compute minimum time
  if(is.null(time.min) || is.na(time.min)) {
    time.min = time.type(min(vapply(X=x,
                                    FUN=function(l) min(l$x[is.finite(l$x)], na.rm = TRUE),
                                    FUN.VALUE = +Inf)));
  }
  # re-compute maximum time
  if(is.null(time.max) || is.na(time.max)) {
    time.max = time.type(max(vapply(X=x,
                                    FUN=function(l) max(l$x[is.finite(l$x)], na.rm = TRUE),
                                    FUN.VALUE = -Inf)));
  }
  # compute quality minimum
  if(is.null(goal.min) || is.na(goal.min)) {
    goal.min = min(vapply(X=x,
                          FUN=function(l) min(l$y[is.finite(l$y)], na.rm = TRUE),
                          FUN.VALUE=+Inf));
  }
  # compute quality maximum
  if(is.null(goal.max) || is.na(goal.max)) {
    goal.max = max(vapply(X=x,
                          FUN=function(l) max(l$y[is.finite(l$y)], na.rm = TRUE),
                          FUN.VALUE=-Inf));
  }

  # fix the line lengths
  x <- lapply(X=x,
              FUN=function(res) {
                x <- res$x;
                y <- res$y;

                # should we add a point in the front?
                if((!(is.null(time.min) || is.na(time.min))) && (x[1L] > time.min)) {
                  x <- c(time.min, x);
                  y <- c(0, y);
                }

                # should we add a point at the end?
                if((!(is.null(time.max) || is.na(time.max))) && (x[length(x)] < time.max)) {
                  x <- c(x, time.max);
                  y <- c(y, y[length(y)]);
                }

                res$x <- x;
                res$y <- y;
                res <- force(res);
                return(res);
              });

  # load the parameters
  params <- list(...);
  params$x <- c(time.min, time.max);
  params$y <- c(goal.min, goal.max);
  params$type <- "n";
  # set the x-axis
  if(is.null(params$xaxs)) {
    params$xaxs <- "i";
  }
  # set the y-axis
  if(is.null(params$yaxs)) {
    params$yaxs <- "i";
  }

  # set the margins
  if(is.null(params$mai)) {
    params$mai <- c(0.01, 0.01, 0.01, 0.01);
  }
  if(is.null(params$omi)) {
    params$omi <- c(0.01, 0.01, 0.01, 0.01);
  }
  if(is.null(params$xlab)) {
    params$xlab <- "";
  }
  if(is.null(params$ylab)) {
    params$ylab <- "";
  }
  if(is.null(params$main)) {
    params["main"] <- list(NULL);
  }
  # invoke the plot
  do.call(plot, params);

  # now prepare the line parameters
  if(is.null(params$lwd)) {
    lwd <- 2;
  } else {
    lwd <- params$lwd;
  }

  # plot the markers
  if(!(is.null(time.markers))) {
    # plot the time markers
    for(m in time.markers) {
      if((m >= time.min) && (m <= time.max)) {
        # ...but only if they are in the right range
        abline(v=m, col="gray", lwd=(lwd/3));
      }
    }
  }
  if(!(is.null(goal.markers))) {
    # plot the goal markers
    for(m in goal.markers) {
      if((m >= goal.min) && (m <= goal.max)) {
        # ...but only if they are in the right range
        abline(h=m, col="gray", lwd=(lwd/3));
      }
    }
  }

  # setup the columns and line types, if they are not provided
  if(is.null(params$col)) {
    col <- colorFun(length(x));
  } else {
    col <- params$col;
  }
  if(is.null(params$lty)) {
    lty <- lineTypeFun(length(x));
  } else {
    lty <- params$lty;
  }

  # now print the lines
  for(i in seq_along(x)) {
    lines(x=x[[i]]$x, y=x[[i]]$y, col=col[[i]], lty=lty[[i]], type="s", lwd=lwd);
  }

  # print the legend if necessary
  if(!(is.null(legend) || is.null(legend.pos))) {
    params <- list(x=legend.pos,
                   legend=legend,
                   col=col,
                   text.col=col,
                   lwd=lwd,
                   bty="n",
                   lty=lty,
                   seg.len = (1.5*lwd));
    if(!(is.null(legend.cex) || is.na(legend.cex))){
      params$cex <- legend.cex;
    }
    do.call(graphics::legend, params);
  }

  return(list(x=c(time.min, time.max), y=c(goal.min, goal.max)));
}
