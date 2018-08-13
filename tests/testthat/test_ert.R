library("plotteR")
context("plot.ert")

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

.tester <- function(data) {
  graphics.off();

  # create destination file
  dest <- tempfile(pattern="plot-test", fileext=".pdf");
  expect_false(file.exists(dest));

  pdf(dest, width=6, height=3);
  plot.ert(data);

  dev.off();

  expect_true(file.exists(dest));
  expect_gt(file.size(dest), 100L);

  # delete temp file
  unlink(dest);
}

test_that("Test plot.ert random runs", {
  .tester(list(make.runs(20, 0.0, 1e6)));
  .tester(list(make.runs(20, 0.0, 1e6),
               make.runs(20, 0.5, 1e6)));
  .tester(list(make.runs(20, 0.0, 1e6),
               make.runs(20, 0.5, 1e6),
               make.runs(20, 0.0, 1e4),
               make.runs(20, 0.5, 1e4),
               make.runs(20, 0.1, 1e5)));
})
