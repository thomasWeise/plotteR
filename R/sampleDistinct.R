# an objective function for creating points distributed far away from (0) and (1)
# by maximizing the minimum inter-point distances
.f <- function(vec) {
  mat  <- matrix(c(0, 0, 0, 1, 1, 1, vec), nrow=3L);
  cols <- 2L + (length(vec) %/% 3L);
  # compute the distance from each point to every other point
  # but do not consider the distance between (0) and (1)
  -sum(vapply(X=seq.int(from=3L, to=cols, by=1L),
             FUN=function(i) {
                min(vapply(X=seq.int(from=1L, to=(i-1L), by=1L),
                            FUN=function(j, i) sum((mat[,i] - mat[,j]) ^ 2),
                            FUN.VALUE = NaN, i=i))
             }, FUN.VALUE = NaN));
}

# Get a matrix of maximally distinct points
#' @importFrom stats runif
#' @importFrom minqa bobyqa
.sampleDistinct<- function(n) {
  dim <- 3L*n;

  par <- runif(n=dim);
  q   <- .f(par);

  for(i in seq_len(3*n)) {
    result <- runif(n=dim);
    result.q   <- .f(result);
    if(result.q < q) {
      par <- result;
      q   <- result.q;
    }
  }

  if(n < 50) {
    suppressWarnings({
      result <- bobyqa(par=par, fn=.f, lower=0, upper=1, control=list(maxfun=3*n));
      if(!(is.null(result))) {
        if(is.finite(result$fval) && (result$feval < 0) && (result$feval < q)) {
          par <- result$par;
        }
      }
    });
  }
  return(matrix(par, nrow=3L));
}
