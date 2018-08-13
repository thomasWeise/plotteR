# the find function which obtains the left-most index i for which
# comparator(vec[i], x) is TRUE
# @param x the value we search
# @param vec the vector we search in
# @param findFun the find function
# @param comparator the comparator we use
# @return -1 if for no element in vec the comparasion with x works, the
#   left-most index where it does otherwise
.find.fun <- function(x, vec, findFun, comparator) {
  # get the length of the vector
  vec.len <- length(vec);
  if(vec.len <= 0L) { return(-1L); }

  # invoke the find function
  i <- findFun(x, vec);

  # did we find something
  if((i < 0L) || (i > vec.len)) {
    return(-1L);  # no, return -1
  }

  # ok, we maybe found something, but also maybe not
  if((i <= 0L) || (!(comparator(vec[[i]], x)))) {
    # maybe found??
    if((i < vec.len) && comparator(vec[[i + 1L]], x)) {
      i <- (i + 1L); # found one step to the right
    } else {
      if((i > 0L) && comparator(vec[[i - 1L]], x)) {
        i <- (i - 1L); # found one step to the left
      } else {
        return(-1L); # not found
      }
    }
  }

  # ok, we found the result
  # try to move upward as long as the comparison remains TRUE
  while((i > 1L) && comparator(vec[[i - 1L]], x)) {
    i <- (i - 1L);
  }

  return(i); # return the index
}

# the find interval function for decreasing values
.nfindInterval <- function(x, vec) findInterval(-x, -vec);
.find.fun.lt <- function(x, vec) .find.fun(x, vec, .nfindInterval, `<`);
.find.fun.lte <- function(x, vec) .find.fun(x, vec, .nfindInterval, `<=`);
.find.fun.gt <- function(x, vec) .find.fun(x, vec, findInterval, `>`);
.find.fun.gte <- function(x, vec) .find.fun(x, vec, findInterval, `>=`);

# make the find function for a specific comparator
.make.findFun <- function(comparator) {
  # create the function for searching in lists
  if(comparator(1L, 2L)) {
    # if the comparator is <= or <, we have a decreasing list
    if(comparator(1L, 1L)) {
      return(.find.fun.lte); # we have <=
    }
    # the comparator is <, we have a decreasing list
    return(.find.fun.lt);
  }

  # otherwise, the comparator should be >= or >, so we have an increasing list
  if(comparator(1L, 1L)) {
    # the comparator should be >=
    return(.find.fun.gte);
  }
  # the comparator should be >
  return(.find.fun.gt);
}

# compute the time maximum
.dim.max <- function(x, dim, type) {
  if(is.null(x) || (length(x) <= 0L)) {
    # handle the case where no data is available
    return(NA_integer_);
  }

  # compute the maximum time value anywhere in x
  return(type(max(vapply(X=x,
         FUN=function(run) {
           dims <- dim(run);
           # the matrix dimension
           if(is.null(dims)) {
             # if the matrix dimensions are null, we have a simple vector or
             # list and can compare the goal dimension directly
             return(type(run[[dim]]));
           }
           # return the time index
           return(type(max(run[, dim])));
         }, FUN.VALUE = type(0)))));
}

# compute the time minimum
.dim.min <- function(x, dim, type) {
  if(is.null(x) || (length(x) <= 0L)) {
    # handle the case where no data is available
    return(NA_integer_);
  }

  # compute the minimum time value anywhere in x
  return(type(min(vapply(X=x,
         FUN=function(run) {
           dims <- dim(run);
           # the matrix dimension
           if(is.null(dims)) {
             # if the matrix dimensions are null, we have a simple vector or
             # list and can compare the goal dimension directly
             return(type(run[[dim]]));
           }
           # return the time index
           return(type(min(run[, dim])));
         }, FUN.VALUE = type(0)))));
}

# find the time when a specific goal is reached
# @param run the run
# @param goal the goal
# @param goal.dim the goal dimension
# @param time.dim the time dimension
# @param time.max the maximum time
# @param time.type the time type
# @param comparator the comparator
# @param findFun the finder function
.find.time <- function(run, goal, goal.dim, time.dim, time.max, time.type, comparator, findFun) {
  # get the success times
  dims <- dim(run);
  # the matrix dimension
  if(is.null(dims)) {
    # if the matrix dimensions are null, we have a simple
    # vector or list and can compare the goal dimension
    # directly
    if(comparator(run[[goal.dim]], goal)) {
      # the goal was found, so return the time dimension
      return(time.type(run[[time.dim]]));
    }
    # the goal was not found, return the negated time
    if(is.null(time.max) || is.na(time.max)) {
      return(time.type(-run[[time.dim]]));
    }
  } else {
    # we have a matrix and need to find the right time value
    i <- findFun(goal, run[, goal.dim]);
    if(i > 0L) { # yeah, we found the time
      return(time.type(run[i, time.dim]));
    }
    # no, we found nothing
    if(is.null(time.max) || is.na(time.max)) {
      return(time.type(-run[dims[1L], time.dim]));
    }
  }
  return(time.type(-time.max));
}
