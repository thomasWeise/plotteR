library("plotteR")
context("plot.ecdf")

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
                  if(i <= m) {
                    if(runif(n=1L) < 0.5) {
                      end <- runif(n=1L, min=-10L, max=0L);
                    } else {
                      end <- 0L;
                    }
                  } else {
                    end <- runif(n=1L, min=1L, max=100L);
                  }
                  return(make.run(end));
                }))
}

test_that("Test plot.ecdf random runs", {
  plot.ecdf(list(make.runs(1, 0)));
  plot.ecdf(list(make.runs(1, 1)));
  plot.ecdf(list(make.runs(1, 0), make.runs(1, 1)));
  plot.ecdf(list(make.runs(10, 0), make.runs(10, 1), make.runs(10, 5), make.runs(10, 10), make.runs(10, 9)));
})
