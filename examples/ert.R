library("plotteR");

set.seed(10000L);

# create a single run
make.run <- function(q.best, time.worst, ...) {
  repeat {
    x <- sort(unique(as.integer(c(1L, time.worst, runif(n=19L, min=1L, max=time.worst)))));
    if(length(x) == 21L) { break; }
  }

  range <- 1 - q.best;
  repeat {
    l <- unique((exp(runif(n=20L, min=1L, max=5L))-exp(1L))/exp(5L));
    if(length(l) != 20L) { next; }
    if(any((l<=0) | (l >= 1))) { next; }
    y <- sort(unique(c(q.best, q.best + (range*l))), decreasing = TRUE);
    if(length(y) == 21L) { break; }
  }
  return(matrix(c(x, y), ncol=2L))
}

# make n runs
make.runs <- function(n, q.best, time.worst) {
  return(lapply(X=seq_len(n), FUN=make.run, q.best=q.best, time.worst=time.worst));
}

# plot five example ERTs
plot.func.ert(x = list(make.runs(20, 0.0, 1e6),
                       make.runs(20, 0.5, 1e6),
                       make.runs(20, 0.0, 1e4),
                       make.runs(20, 0.5, 1e4),
                       make.runs(20, 0.1, 1e5)),
              legend=c("slow+good", "slow+bad", "fast+good", "fast+bad", "ok+ok"),
              log="y",
              time.max=1e6,
              goal.min=0,
              goal.max=1,
              goal.markers = c(0.1, 0.5),
              time.markers = c(1e6, 1e4, 1e5));
