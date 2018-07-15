
# make a vector where all possible combinations of (i, j) with i, j in 1..n
# occur
.make.pairs <- function(n) {
  return(unlist(lapply(X=1L:(n-1L),
                       FUN=function(i) {
                            lapply(X=(i+1L):n,
                                   FUN=function(j, i) c(i, j), i)
                       }), recursive = TRUE))
}

# reduce all sub-sequence of type (i, i) to i
.reduce.pairs.AA <- function(pairs, n) {
  return(pairs[vapply(X=1L:n,
                      FUN=function(i) {
                            if(i <= 1L) { return(TRUE); }
                            else { return(pairs[i] != pairs[i-1L]); }
                          },
                      FUN.VALUE = FALSE)]);
}

# reduce one tuple of type ABA to A if there is a sequence of type AB or BA
# somewhere else
.reduce.pairs.ABA <- function(pairs, n) {
  for(i in 2L:(n-1L)) {
    A <- pairs[i - 1L];
    B <- pairs[i];
    C <- pairs[i + 1L];

    if(A == C) {
      for(j in 1L:(n-1L)) {
        if(j != i)  {
          if((pairs[j] == B) && (pairs[j + 1L] == C)) {
            pairs <- pairs[-i];
            return(pairs[-i]);
          }
        }
      }

      if(A == C) {
        for(j in 2L:n) {
          if(j != i)  {
            if((pairs[j] == B) && (pairs[j - 1L] == A)) {
              pairs <- pairs[-i];
              return(pairs[-i]);
            }
          }
        }
      }
    }
  }

  return(pairs);
}

# reduce one tuple of type ABC to B if there is a sequence of type AB and BC
# somewhere else
.reduce.pairs.ABC <- function(pairs, n) {
  for(i in 2L:(n-1L)) {
    A <- pairs[i - 1L];
    B <- pairs[i];
    C <- pairs[i + 1L];

    foundAB <- FALSE;
    foundBC <- FALSE;
    for(j in 2L:n) {
      if(j != i) {
        if( ((B == pairs[j]) && (A == pairs[j - 1L])) ) {
          foundAB <- TRUE;
          break;
        }
      }
    }

    for(j in 1L:(n-1L)) {
      if(j != i) {
        if( ((B == pairs[j]) && (C == pairs[j + 1L])) ) {
          foundBC <- TRUE;
          break;
        }
      }
    }

    if(foundAB && foundBC) {
      return(pairs[-j]);
    }
  }
  return(pairs);
}

# reduce the end AB to A if AB occurs elsewhere
.reduce.pairs.end <- function(pairs, n) {
  A <- pairs[n-1L];
  B <- pairs[n];
  for(i in 1L:(n-2L)) {
    A2 <- pairs[i];
    B2 <- pairs[i+1L];
    if(((A2 == A) && (B2 == B)) ||
       ((A2 == B) && (B2 == A))) {
      return(pairs[-n]);
    }
  }
  return(pairs);
}

# reduce the start AB to B if AB occurs elsewhere
.reduce.pairs.start <- function(pairs, n) {
  A <- pairs[1L];
  B <- pairs[2L];
  for(i in 2L:(n-1L)) {
    A2 <- pairs[i];
    B2 <- pairs[i+1L];
    if(((A2 == A) && (B2 == B)) ||
       ((A2 == B) && (B2 == A))) {
      return(pairs[-n]);
    }
  }
  return(pairs);
}

# reduce the pairs
.reduce.pairs <- function(pairs) {
  n <- length(pairs);
  repeat {
    n.old <- n;
    pairs <- .reduce.pairs.AA(pairs, n);
    n <- length(pairs);
    if(n <= 2L) { break; }
    pairs <- .reduce.pairs.ABA(pairs, n);
    n <- length(pairs);
    if(n <= 2L) { break; }
    pairs <- .reduce.pairs.ABC(pairs, n);
    n <- length(pairs);
    if(n <= 2L) { break; }
    pairs <- .reduce.pairs.start(pairs, n);
    n <- length(pairs);
    if(n <= 2L) { break; }
    pairs <- .reduce.pairs.end(pairs, n);
    n <- length(pairs);
    if(n <= 2L) { break; }
    if(n.old >= n) { break; }
  }
  return(pairs);
}

#' @title Plot a Vector of Colors so that we can see if they are unique
#' @description Take a vector of colors \code{x} and plot them in a way so that
#'   each pair of colors occurs once. This allows us to see if they are unique
#'   and distinct.
#' @param x the colors to plot
#' @param ... the parameters passed to \code{pie} or \code{plot}
#' @importFrom graphics pie plot rect
#' @export plot.colors
plot.colors <- function(x, ...) {
  x <- unlist(x, recursive = TRUE);
  n <- length(x);

  if(n <= 3L) {
    # if there are only few colors, plot them in a pie chart
    pie(x=rep(1, n), labels=NA, col=x,
        radius=1, border=FALSE, lty=0, ...);
  } else {
    # get the pairs of colors
    pairs <- .make.pairs(n);
    pairs <- .reduce.pairs(pairs);
    n <- length(pairs);

    plot(x=c(0,n), y=c(0,1), type="n", xlab="", ylab="", ann=FALSE,
         asp=FALSE, bty="n", xaxt="n", yaxt="n", ...);

    for(i in 1L:n){
      rect((i-1L), 1, i, 0, col=x[pairs[i]], border=NA);
    }
  }
}

