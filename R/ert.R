#' @title Plot ERT Functions
#' @description Plot the Expected Running Time (ERT) functions for a couple of
#'   different datasets.
#' @param x the data, maybe a list of [lists of matrices or a list of
#'   vectors/lists], where each element has at least the \code{goal.dim} and
#'   \code{time.dim} dimension.
#' @param goal.dim the dimension where where the goal values can be found
#' @param time.dim the dimension where the time values can be found
#' @param comparator the comparator, usually \code{`<=`} or \code{`>=`}
#' @param time.type the type function the time dimension, should be
#'   \code{as.integer} or \code{as.numeric}
#' @param goal.type the type function the goal dimension, should be
#'   \code{as.integer} or \code{as.numeric}
#' @param time.max the maximum time value to be used, or \code{NA} to pick the
#'   maximum time value of any run
#' @param goal.min the minimum goal value or \code{NA} to pick the minimum time
#'   value
#' @param goal.max the maximum goal value or \code{NA} to pick the minimum time
#'   value
#' @param extract.runs a function which can be used to extract the run sets from
#'   \code{x}, e.g., \code{identity}
#' @param extract.run a function which can be used to extract the single runs
#'   from the run sets, e.g., \code{identity}
#' @param lineTypeFun the line type function, a function returning the line
#'   types to use for a provided number of inputs
#' @param colorFun the colors function, a function returning the color list to
#'   use for a provided number of inputs
#' @param legend the legend names
#' @param legend.pos the legend position (optional, default: bottomleft)
#' @param legend.cex the character sizing for the legend, optional, default
#'   \code{NA}
#' @param goal.markers markers for the goal values
#' @param time.markers markers for the time values
#' @return a \code{list(x=c(goal.min, goal.max), y=c(time.min, time.max))} of
#'   the minimum and maximum time and goal values encountered
#' @inheritDotParams graphics::plot
#' @export plot.func.ert
#' @include ertFun.R
#' @include funUtils.R
#' @include distinctLineTypes.R
#' @include distinctColors.R
#' @importFrom graphics plot abline legend lines
#' @example examples/ert.R
plot.func.ert <- function(x,
                          goal.dim=2L,
                          time.dim=1L,
                          time.type=as.integer,
                          goal.type=as.numeric,
                          goal.min=goal.type(NA_real_),
                          goal.max=goal.type(NA_real_),
                          time.max=time.type(NA_real_),
                          extract.run=identity,
                          extract.runs=identity,
                          comparator=`<=`,
                          lineTypeFun=lineTypes.distinct,
                          colorFun=colors.distinct,
                          goal.markers=c(0.2,0.4,0.6,0.8),
                          time.markers=NULL,
                          legend=NULL,
                          legend.pos="bottomleft",
                          legend.cex=NA,
                          ...) {

  # x becomes a list of lists
  if(!(identical(extract.runs, identity) &&
       identical(extract.run, identity))) {
    x <- lapply(X=x, FUN=function(runs) {
            lapply(X=extract.runs(runs), FUN=extract.run)
          });
  }

  # get the global goal minimum
  if(is.null(goal.min) || is.na(goal.min)) {
    goal.min <- vapply(X=x,
                       FUN=.dim.min,
                       FUN.VALUE=goal.type(NA),
                       dim=goal.dim,
                       type=goal.type);
    if(all(is.null(goal.min) | is.na(goal.min))) {
      goal.min <- goal.type(NA);
    } else {
      goal.min <- goal.type(min(goal.min));
    }
  }

  # get the global goal maximum
  if(is.null(goal.max) || is.na(goal.max)) {
    goal.max <- vapply(X=x,
                       FUN=.dim.max,
                       FUN.VALUE=goal.type(NA),
                       dim=goal.dim,
                       type=goal.type);
    if(all(is.null(goal.max) | is.na(goal.max))) {
      goal.max <- goal.type(NA);
    } else {
      goal.max <- goal.type(max(goal.max));
    }
  }

  # get the global time maximum
  if(is.null(time.max) || is.na(time.max)) {
    time.max <- vapply(X=x,
                       FUN=.dim.max,
                       FUN.VALUE=NA_integer_,
                       dim=time.dim,
                       type=time.type);
    if(all(is.null(time.max) | is.na(time.max))) {
      time.max <- NA_integer_;
    } else {
      time.max <- time.type(max(time.max));
    }
  }

  # compute the data
  x <- lapply(X=x,
              FUN=func.ert,
              goal.dim=goal.dim,
              time.dim=time.dim,
              comparator=comparator,
              time.type=time.type,
              goal.type=goal.type,
              goal.min=goal.min,
              goal.max=goal.max,
              time.max=time.max,
              extract.run=identity);

  # compute quality minimum
  if(is.null(goal.min) || is.na(goal.min)) {
    goal.min = goal.type(min(vapply(X=x,
                             FUN=function(l) min(l$x),
                             FUN.VALUE=+Inf)));
  }
  # compute quality maximum
  if(is.null(goal.max) || is.na(goal.max)) {
    goal.max = goal.type(max(vapply(X=x,
                             FUN=function(l) max(l$x),
                             FUN.VALUE=-Inf)));
  }

  time.min <- time.type(min(vapply(X=x,
                                   FUN=function(l) min(l$y),
                                   FUN.VALUE=+Inf)));
  time.max <- time.type(max(vapply(X=x,
                                   FUN=function(l) {
                                     y <- is.finite(l$y);
                                     if(any(y)) {
                                       return(max(l$y[y]));
                                     }
                                     return(-Inf);
                                   },
                                   FUN.VALUE=-Inf)));

  # load the parameters
  params <- list(...);
  params$x <- c(goal.min, goal.max);
  params$y <- c(time.min, time.max);
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
        abline(h=m, col="gray", lwd=(lwd/3));
      }
    }
  }
  if(!(is.null(goal.markers))) {
    # plot the goal markers
    for(m in goal.markers) {
      if((m >= goal.min) && (m <= goal.max)) {
        # ...but only if they are in the right range
        abline(v=m, col="gray", lwd=(lwd/3));
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
    y <- x[[i]]$y;
    y[!(is.finite(y))] <- .Machine$double.xmax;
    lines(x=x[[i]]$x, y=y, col=col[[i]], lty=lty[[i]], type="s", lwd=lwd);
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

  return(list(x=c(goal.min, goal.max), y=c(time.min, time.max)));
}
