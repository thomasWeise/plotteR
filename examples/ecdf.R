library("plotteR");

set.seed(10000L);
time.max.pow <- 8;

# create a single run, where the quality dimension reaches to end
make.run <- function(end) {
  repeat {
    x <- sort(unique(as.integer(runif(n=20L, min=1L, max=(10^(runif(n=1L, min=2, max=time.max.pow)))))));
    if(length(x) == 20L) { break; }
  }
  repeat {
    y <- sort(unique(c(end, runif(n=19L, min=end, max=500))), decreasing=TRUE);
    if(length(y) == 20L) {
      break;
    }
  }
  return(matrix(c(x, y), ncol=2L))
}

# make n runs where m reach below 0, i.e., whose ECDF reaches m/n
make.runs <- function(n, m) {
  return(lapply(X=seq_len(n),
                FUN=function(i) {
                  if(i <= m) { end <- runif(n=1L, min=-10L, max=0L); }
                  else       { end <- runif(n=1L, min=1L, max=100L); }
                  return(make.run(end));
                }))
}

# plot five example ECDFs, where the end results reach 3/20, 10/20, 5/20, 15/20,
# and 19/20, respectively
plot.func.ecdf(x = list(make.runs(20, 3),
                        make.runs(20, 10),
                        make.runs(20, 5),
                        make.runs(20, 15),
                        make.runs(20, 19)),
               names=c("worst", "good", "bad", "better", "best"),
               time.markers=c(1e2, 1e4, 1e6, 1e8),
               log="x",
               time.max=(10^time.max.pow));
