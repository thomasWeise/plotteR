#' @title Plot a Vector of Colors so that we can see if they are unique
#' @description Take a vector of \code{colors} and plot them in a way so that
#'   each pair of colors occurs once. This allows us to see if they are unique
#'   and distinct.
#' @param colors the colors to plot
#' @importFrom graphics pie plot polygon
#' @export plot.colors
plot.colors <- function(colors) {
  colors <- unlist(colors, recursive = TRUE);
  n <- length(colors);

  if(n <= 3L) {
    # if there are only few colors, plot them in a pie chart
    pie(x=rep(1, n), labels=NA, col=colors,
        radius=1, border=FALSE, lty=0);
  } else {
    # get the pairs of colors
    pairs <- unlist(lapply(X=1L:(n-1L),
                           FUN=function(i) {
                             lapply(X=(i+1L):n,
                                    FUN=function(j, i) c(i, j), i)
                           }), recursive = FALSE);
    pairs.n <- length(pairs);

    n.x <- as.integer(ceiling(sqrt(pairs.n)) + 0.1);
    n.y <- as.integer(ceiling(pairs.n / n.x) + 0.1);

    plot(x=c(0,n.x), y=c(0,n.y), type="n", xlab="", ylab="", ann=FALSE,
         asp=TRUE, bty="n", xaxt="n", yaxt="n");

    i <- 1L;
    for(b in seq_len(n.y)) {
      for(a in seq_len(n.x)) {
        if(i <= pairs.n) {
          pair <- pairs[[i]];
          x2 <- a;
          x1 <- (x2 - 1L);
          y2 <- n.y - b;
          y1 <- y2 + 1L;

          polygon(x=c(x1, x2, x2),
                  y=c(y1, y1, y2),
                  col=colors[pair[1L]],
                  lty=c(0,0));
          polygon(x=c(x1, x1, x2),
                  y=c(y1, y2, y2),
                  col=colors[pair[2L]],
                  lty=c(0,0));
          i <- i + 1L;
        }
      }
    }
  }
}

