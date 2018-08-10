.nfindInterval <- function(x, vec) findInterval(-x, -vec);

# compute the time maximum
.time.max <- function(x, timeDim, timeType) {

  if(is.null(x) || (length(x) <= 0L)) {
    # handle the case where no data is available
    return(NA_integer_);
  }

  # compute the maximum time value anywhere in x
  return(timeType(max(vapply(X=x,
                             FUN=function(run) {
                               dims <- dim(run);
                               # the matrix dimension
                               if(is.null(dims)) {
                                 # if the matrix dimensions are null, we have a simple vector or list and can compare the goal dimension directly
                                 return(timeType(run[[timeDim]]));
                               }
                               # return the time index
                               return(timeType(max(run[, timeDim])));
                             }, FUN.VALUE = timeType(0)))));
}

# compute the time minimum
.time.min <- function(x, timeDim, timeType) {

  if(is.null(x) || (length(x) <= 0L)) {
    # handle the case where no data is available
    return(NA_integer_);
  }

  # compute the minimum time value anywhere in x
  return(timeType(min(vapply(X=x,
                             FUN=function(run) {
                               dims <- dim(run);
                               # the matrix dimension
                               if(is.null(dims)) {
                                 # if the matrix dimensions are null, we have a simple vector or list and can compare the goal dimension directly
                                 return(timeType(run[[timeDim]]));
                               }
                               # return the time index
                               return(timeType(min(run[, timeDim])));
                             }, FUN.VALUE = timeType(0)))));
}


#' @title Create an ECDF Function
#' @description Create an ECDF function, i.e., a list with two membery \code{x}
#'   and \code{y} that stand for the coordinates of the function.
#' @param x the data, maybe a list of matrices or a list of vectors/lists, where
#'   each element has at least the \code{goalDim} and \code{timeDim} dimension.
#' @param goal the goal value, i.e., the value which must be reached to be
#'   counted as success
#' @param goalDim the dimension where where the goal values can be found
#' @param timeDim the dimension where the time values can be found
#' @param comparator the comparator, usually \code{`<=`} or \code{`>=`}
#' @param timeType the type function the time dimension, should be
#'   \code{as.integer} or \code{identity}
#' @param time.min the minimum time value to be used for the diagram, or
#'   \code{NA} to use the smallest time value in any run
#' @param time.max the maximum time value to be used, or \code{NA} to pick the
#'   maximum time value of any run
#' @param extract.run a function which can be used to extract the single runs
#'   from \code{x}, e.g., \code{identity}
#' @return a \code{list(x=c(...), y=c(...))} where the two members \code{x} and
#'   \code{y} will be vectors of corresponding coordinates
#' @export func.ecdf
func.ecdf <- function(x,
                      goal=0,
                      goalDim=2L,
                      timeDim=1L,
                      comparator=`<=`,
                      timeType=as.integer,
                      time.min=1L,
                      time.max=NA_integer_,
                      extract.run=identity) {

  # first, get the total number of runs
  runs <- length(x);
  stopifnot(runs > 0L, is.finite(runs), length(runs)==1L);

  # extract the single runs
  if(!(identical(extract.run, identity))) {
    x <- lapply(X=x, FUN=extract.run);
  }

  # create the function for searching in lists
  if(comparator(1L, 2L)) {
    # if the comparator is <= or <, we have a decreasing list
    findFun <- .nfindInterval;
  } else {
    # otherwise, the comparator should be >= or >, so we have an increasing list
    findFun <- findInterval;
  }

  # make sure we have a proper time minimum
  if(is.na(time.min) || is.null(time.min)) {
    time.min <- .time.min(x=x,
                          timeDim=timeDim,
                          timeType=timeType);
  }

  # make sure we have a proper time maximum
  if(is.na(time.max) || is.null(time.max)) {
    time.max <- .time.max(x=x,
                          timeDim=timeDim,
                          timeType=timeType);
  }

  # get the success times
  times <- timeType(sort(vapply(X=x,
                    FUN=function(run) {
                      dims <- dim(run);
                      # the matrix dimension
                      if(is.null(dims)) {
                        # if the matrix dimensions are null, we have a simple vector or list and can compare the goal dimension directly
                        if(comparator(run[[goalDim]], goal)) {
                          # the goal was found, so return the time dimension
                          return(timeType(run[[timeDim]]));
                        }
                        # the goal was not found, return NA
                        return(NA_integer_);
                      }

                      # we have a matrix and need to find the right time value
                      i <- findFun(goal, run[, goalDim]);
                      if((i <= 0L) || (!comparator(run[i, goalDim], goal))) {
                        # not found
                        return(NA_integer_);
                      }

                      # try to move upward as long as the comparison remains TRUE
                      while((i > 1L) && (comparator(run[(i-1L), goalDim], goal))) {
                        i <- (i - 1L);
                      }

                      # return the time index
                      return(timeType(run[i, timeDim]));
                    }, FUN.VALUE = timeType(0)), na.last=NA));

  len <- length(times);
  if(len <= 0L) {
    # handle the trivial case where there was no success at all
    if(is.null(time.min) || is.na(time.min)) { # no min
      if(is.null(time.max) || is.na(time.max)) { # no max
        return(list(x=integer(0), y=numeric(0))); # return empty
      }
      return(list(x=c(time.max), y=c(0)));
    } # we have min
    if((is.null(time.max) || is.na(time.max)) || (time.max <= time.min)) {
      return(list(x=c(time.min), y=c(0))); # and a different max
    } # else: we have both
    return(list(x=c(time.min, time.max), y=c(0, 0)));
  }

  # use rle to get the runs
  encoding <- rle(times);
  x <- timeType(encoding$values);
  y <- cumsum(encoding$lengths) / runs;

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

  # return the list
  return(list(x=timeType(x), y=y));
}
