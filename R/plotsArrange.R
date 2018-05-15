# the internal objective function
#' @include goldenRatio.R
.plot.ij.objective <- function(i, j, n) {
  if((i <= 0) || (j <= 0) ||
     (!(is.finite(i) && is.finite(j)))) { return(+Inf); }

  total <- (i*j);
  if((!(is.finite(total))) || (total < n)) { return(+Inf); }

  a <- min(i, j);
  b <- max(i, j);

  # the deviation from the golden ratio
  goldenRatioDeviation <- abs(((b / a) - .goldenRatio) / .goldenRatio);

  # the deviation from full rows
  fullLastRowDeviation <- (total - n) / b;

  # overlong row deviation
  if(b > 5) { overlong <- (b - 5); }
  else      { overlong <- 0; }

  return((2*(goldenRatioDeviation^2)) + fullLastRowDeviation + (overlong*2));
}

#' @title Find a Proper Numbers of Rows and Columns for \code{n} Plots
#' @description Given a number \code{n} of plots, find proper numbers of rows and columns in which these plots can be arranged.
#' @param n the number of plots
#' @return a vector \code{c(rows, columns)} with the number of rows and columns in which these plots can be arranged.
#' @export plots.arrange
plots.arrange <- function(n) {
  stopifnot(is.finite(n));
  n <- as.integer(n);

  if(n <= 1) { return(c(1L, 1L)); }
  if(n <= 2) { return(c(1L, 2L)); }
  if(n <= 3) { return(c(1L, 3L)); }
  if(n <= 4) { return(c(2L, 2L)); }

  best.i <- n;
  best.j <- 1L;
  best.f <- .Machine$double.xmax;
  for(i in 2L:n) {
    j <- as.integer(n/i);
    if((j*i) < n) { j <- j + 1L; }
    f <- .plot.ij.objective(i, j, n);
    if(f < best.f) {
      best.i <- min(i, j);
      best.j <- max(i, j);
      best.f <- f;
    }
  }

  return(as.integer(c(best.i, best.j)));
}
