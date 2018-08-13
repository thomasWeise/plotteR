#' @title Create an ERT Function
#' @description Create an Expected Running Time (ERT) function, i.e., a list
#'   with two membery \code{x} and \code{y} that stand for the coordinates of
#'   the function.
#' @param x the data, maybe a list of matrices or a list of vectors/lists, where
#'   each element has at least the \code{goal.dim} and \code{time.dim}
#'   dimension.
#' @param goal.dim the dimension where where the goal values can be found
#' @param time.dim the dimension where the time values can be found
#' @param comparator the comparator, usually \code{`<=`} or \code{`>=`}
#' @param time.type the type function the time dimension, should be
#'   \code{as.integer} or \code{as.numeric}
#' @param goal.type the type function the goal dimension, should be
#'   \code{as.integer} or \code{as.numeric}
#' @param time.max the maximum time value to be used, or \code{NA} to pick the
#'   maximum time value of each run separately
#' @param goal.min the minimum goal value to use, or \code{NA} to use the
#'   minimal occuring value
#' @param goal.max the maximum goal value to use, or \code{NA} to use the
#'   maximally occuring one
#' @param extract.run a function which can be used to extract the single runs
#'   from \code{x}, e.g., \code{identity}
#' @return a \code{list(x=c(...), y=c(...))} where the two members \code{x} and
#'   \code{y} will be vectors of corresponding coordinates
#' @export func.ert
#' @include funUtils.R
func.ert <- function(x,
                     goal.dim=2L,
                     time.dim=1L,
                     comparator=`<=`,
                     time.type=as.integer,
                     goal.type=as.numeric,
                     goal.min=goal.type(NA),
                     goal.max=goal.type(NA),
                     time.max=time.type(NA),
                     extract.run=identity) {

  # first, get the total number of runs
  runs <- length(x);
  stopifnot(runs > 0L, is.finite(runs), length(runs)==1L);

  # extract the single runs
  if(!(identical(extract.run, identity))) {
    x <- lapply(X=x, FUN=extract.run);
  }

  # create the function for searching in lists
  findFun <- .make.findFun(comparator);

  # compute the goal values
  goals <- goal.type(sort.int(unique(unlist(#
                  lapply(X=x,
                  FUN=function(run) {
                    dims <- dim(run);
                    # the matrix dimension
                    if(is.null(dims)) {
                      # if the matrix dimensions are null, we have a simple
                      # vector or list and can compare the goal dimension
                      # directly
                      return(goal.type(run[[goal.dim]]));
                    }
                    return(goal.type(run[, goal.dim]));
                  }))), na.last=NA));

  # make sure we have a proper time minimum
  if(is.na(goal.min) || is.null(goal.min)) {
    goal.min <- goals[1L];
  } else {
    if(goal.min < goals[1L]) {
      goals <- goal.type(c(goal.min, goals));
    }
  }

  goals.len <- length(goals);
  # make sure we have a proper time maximum
  if(is.na(goal.max) || is.null(goal.max)) {
    goal.max <- goals[goals.len];
  } else {
    if(goal.max > goals[goals.len]) {
      goals <- goal.type(c(goals, goal.max));
    }
  }

  # get the success times
  y <- vapply(X=goals,
              FUN=function(goal) {

                # compute the necessary data for the ert:
                # 1. whether a run has reached the goal
                # 2. the time spent with goal values worse than the goal
                # We encode this by making the times positive upon success and negative upon failure
                vec <- vapply(X=x, FUN=.find.time,
                              FUN.VALUE = NA_integer_,
                              goal=goal,
                              goal.dim=goal.dim,
                              time.dim=time.dim,
                              time.max=time.max,
                              time.type=time.type,
                              comparator=comparator,
                              findFun=findFun);

                # count successes
                succ <- sum(vec >= 0);
                if(succ <= 0) { return(+Inf); }

                # ok, there is at least one success
                vec <- abs(vec);
                # first compute in numeric field
                vecs <- sum(as.numeric(vec));
                if((vecs < .Machine$integer.max) && (identical(time.type, as.integer))) {
                  # compute in integer range
                  vecs <- as.integer(sum(as.integer(vec)));
                }

                # return expected running time
                return(vecs / succ);
              },
            FUN.VALUE=+Inf);

  # check if we can convert the y into the time type
  if(all(is.finite(y)) && (!(identical(y, identity)))) {
    suppressWarnings(yt <- time.type(y));
    if(all(yt == y)) {
      y <- yt;
    }
  }

  # return the list
  return(list(x=goals, y=y));
}
