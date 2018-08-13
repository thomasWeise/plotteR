#' @title Create an ECDF Function
#' @description Create an Empirical Cumulative Distribution Function (ECDF)
#'   function, i.e., a list with two membery \code{x} and \code{y} that stand
#'   for the coordinates of the function.
#' @param x the data, maybe a list of matrices or a list of vectors/lists, where
#'   each element has at least the \code{goal.dim} and \code{time.dim} dimension.
#' @param goal the goal value, i.e., the value which must be reached to be
#'   counted as success
#' @param goal.dim the dimension where where the goal values can be found
#' @param time.dim the dimension where the time values can be found
#' @param comparator the comparator, usually \code{`<=`} or \code{`>=`}
#' @param time.type the type function the time dimension, should be
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
#' @include funUtils.R
func.ecdf <- function(x,
                      goal=0,
                      goal.dim=2L,
                      time.dim=1L,
                      comparator=`<=`,
                      time.type=as.integer,
                      time.min=1L,
                      time.max=time.type(NA_integer_),
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

  # make sure we have a proper time minimum
  if(is.na(time.min) || is.null(time.min)) {
    time.min <- .dim.min(x=x,
                         dim=time.dim,
                         type=time.type);
  }

  # make sure we have a proper time maximum
  if(is.na(time.max) || is.null(time.max)) {
    time.max <- .dim.max(x=x,
                         dim=time.dim,
                         type=time.type);
  }

  # get the success times
  times <- vapply(X=x, FUN=.find.time,
                  FUN.VALUE = NA_integer_,
                  goal=goal,
                  goal.dim=goal.dim,
                  time.dim=time.dim,
                  time.max=time.max,
                  time.type=time.type,
                  comparator=comparator,
                  findFun=findFun);
  times[times < 0] <- NA_integer_;
  times <- time.type(sort.int(times, na.last=NA));

  # we do this twice, to capture the cut-off of the list of times at the maximum
  # time as well
  for(i in 1L:2L) {
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

    # reduce to only the points in the range
    if(!(is.na(time.max) || is.null(time.max))) {
      times <- times[times <= time.max];
    } else {
      break;
    }
  }

  # use rle to get the runs
  encoding <- rle(times);
  x <- time.type(encoding$values);
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
  return(list(x=time.type(x), y=y));
}
