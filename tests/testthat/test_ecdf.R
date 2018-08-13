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

.tester <- function(data) {
  graphics.off();

  # create destination file
  dest <- tempfile(pattern="plot-test", fileext=".pdf");
  expect_false(file.exists(dest));

  pdf(dest, width=6, height=3);
  plot.ecdf(data);

  dev.off();

  expect_true(file.exists(dest));
  expect_gt(file.size(dest), 100L);

  # delete temp file
  unlink(dest);
}

test_that("Test plot.ecdf random runs", {
  .tester(list(make.runs(1, 0)));
  .tester(list(make.runs(1, 1)));
  .tester(list(make.runs(1, 0), make.runs(1, 1)));
  .tester(list(make.runs(10, 0), make.runs(10, 1), make.runs(10, 5), make.runs(10, 10), make.runs(10, 9)));
})
