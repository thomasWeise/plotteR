# an objective function for creating points distributed far away from (0) and (1)
# by maximizing the minimum inter-point distances
.f <- function(vec) {
  mat  <- matrix(c(0, 0, 0, 1, 1, 1, vec), nrow=3L);
  cols <- 2L + (length(vec) %/% 3L);
  -sum(vapply(X=seq.int(from=3L, to=cols, by=1L),
             FUN=function(i) {
                min(vapply(X=seq.int(from=1L, to=(i-1L), by=1L),
                            FUN=function(j, i) sum((mat[,i] - mat[,j]) ^ 2),
                            FUN.VALUE = NaN, i=i))
             }, FUN.VALUE = NaN));
}

# Get a matrix of maximally distinct points
#' @importFrom minqa bobyqa
.sampleDistinct<- function(n) {
  dim <- 3L*n;
  par <- runif(n=dim);
  q   <- .f(par);
  for(i in 1:5) {
    result <- bobyqa(par=runif(n=dim),
                     fn=.f,
                     lower=rep(x=0, times=dim),
                     upper=rep(x=1, times=dim));
    if(!(is.null(result))) {
      if(is.finite(result$fval) && all(is.finite(result$par)) && (result$fval < q)) {
        par <- result$par;
        q   <- result$fval;
      }
    }
  }
  return(matrix(par, nrow=3L));
}
