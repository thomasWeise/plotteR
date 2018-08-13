#' @title Plot ECDF Functions
#' @description Plot the Empirical Cumulative Distribution Functions (ECDFs) for
#'   a couple of different datasets.
#' @param x the data, maybe a list of [lists of matrices or a list of
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
#' @param extract.runs a function which can be used to extract the run sets from
#'   \code{x}, e.g., \code{identity}
#' @param extract.run a function which can be used to extract the single runs
#'   from the run sets, e.g., \code{identity}
#' @param lineTypeFun the line type function, a function returning the line
#'   types to use for a provided number of inputs
#' @param colorFun the colors function, a function returning the color list to
#'   use for a provided number of inputs
#' @param names the legend
#' @param goal.markers markers for the goal values
#' @param time.markers markers for the time values
#' @inheritDotParams graphics::plot
#' @export plot.ecdf
#' @include ecdfFun.R
#' @include funUtils.R
#' @include distinctLineTypes.R
#' @include distinctColors.R
#' @importFrom graphics plot abline legend lines
plot.ecdf <- function(x,
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
                      names=NULL,
                      goal.markers=c(0.2,0.4,0.6,0.8),
                      time.markers=NULL,
                      ...) {

  # x becomes a list of lists
  if(!(identical(extract.runs, identity) &&
       identical(extract.run, identity))) {
    x <- lapply(X=x, FUN=function(runs) {
            lapply(X=extract.runs(runs), FUN=extract.run)
          });
  }

  # get the global time minimum
  if(is.null(time.min) || is.na(time.min)) {
    time.min <- vapply(X=x,
                       FUN=.dim.min,
                       FUN.VALUE=time.type(NA),
                       dim=time.dim,
                       type=time.type);
    if(all(is.null(time.min) | is.na(time.min))) {
      time.min <- time.type(NA);
    } else {
      time.min <- time.type(min(time.min));
    }
  }

  # get the global time maximum
  if(is.null(time.max) || is.na(time.max)) {
    time.max <- vapply(X=x,
                       FUN=.dim.max,
                       FUN.VALUE=time.type(NA),
                       dim=time.dim,
                       type=time.type);
    if(all(is.null(time.max) | is.na(time.max))) {
      time.max <- time.type(NA);
    } else {
      time.max <- time.type(max(time.max));
    }
  }

  # compute the data
  x <- lapply(X=x,
              FUN=func.ecdf,
              goal=goal,
              goal.dim=goal.dim,
              time.dim=time.dim,
              comparator=comparator,
              time.min=time.min,
              time.max=time.max,
              time.type=time.type);

  # re-compute minimum time
  if(is.null(time.min) || is.na(time.min)) {
    time.min = time.type(min(vapply(X=x,
                             FUN=function(l) min(l$x),
                             FUN.VALUE = +Inf)));
  }
  # re-compute maximum time
  if(is.null(time.max) || is.na(time.max)) {
    time.max = time.type(max(vapply(X=x,
                             FUN=function(l) max(l$x),
                             FUN.VALUE = -Inf)));
  }
  # compute quality minimum
  if(is.null(goal.min) || is.na(goal.min)) {
    goal.min = min(vapply(X=x,
                          FUN=function(l) min(l$y),
                          FUN.VALUE=+Inf));
  }
  # compute quality maximum
  if(is.null(goal.max) || is.na(goal.max)) {
    goal.max = max(vapply(X=x,
                          FUN=function(l) max(l$y),
                          FUN.VALUE=-Inf));
  }

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
  } else {
    params$main <- NULL;
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

  # print the legend if necessar
  if(!(is.null(names))) {
    legend(x="topleft",
           legend=names,
           col=col,
           text.col=col,
           lwd=lwd,
           bty="n",
           lty=lty,
           seg.len = (1.5*lwd));
  }
}
