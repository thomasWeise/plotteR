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
    pairs <- unlist(lapply(X=1L:(n-1L),
                           FUN=function(i) {
                             lapply(X=(i+1L):n,
                                    FUN=function(j, i) c(i, j), i)
                           }), recursive = TRUE);

    n <- length(pairs);
    repeat {
      if(n <= 2L) { break; }

      pairs <- pairs[vapply(X=1L:n,
                      FUN=function(i) {
                        if(i <= 1L) { return(TRUE); }
                        else { return(pairs[i] != pairs[i-1L]); }
                      }, FUN.VALUE = FALSE)];

      n <- length(pairs);
      if(n <= 2L) { break; }

      for(i in 2L:(n-1L)) {
        A <- pairs[i - 1L];
        B <- pairs[i];
        C <- pairs[i + 1L];

        if(A == C) {
          skip <- FALSE;
          for(j in 1L:(n-1L)) {
            if(j != i)  {
              if((pairs[j] == B) && (pairs[j + 1L] == C)) {
                pairs <- pairs[-i];
                pairs <- pairs[-i];
                skip <- TRUE;
                break;
              }
            }
          }
          if(skip) { break; }

          if(A == C) {
            for(j in 2L:n) {
              if(j != i)  {
                if((pairs[j] == B) && (pairs[j - 1L] == A)) {
                  pairs <- pairs[-i];
                  pairs <- pairs[-i];
                  skip <- TRUE;
                  break;
                }
              }
            }
          }
          if(skip) { break; }
        }

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
          pairs <- pairs[-j];
          break;
        }
      }

      n.old <- n;
      n     <- length(pairs);
      if(n >= n.old) { break; }
    }

    plot(x=c(0,n), y=c(0,1), type="n", xlab="", ylab="", ann=FALSE,
         asp=FALSE, bty="n", xaxt="n", yaxt="n", ...);

    for(i in 1L:n){
      rect((i-1L), 1, i, 0, col=x[pairs[i]], border=NA);
    }
  }
}

