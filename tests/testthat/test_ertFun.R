library("plotteR")
context("func.ert")

test_that("Test func.ert (1)", {
  data <- list(c(1, 2));
  res <- func.ert(x=data);
  expect_identical(res, list(x=c(2), y=c(1L)));

  data <- list(c(1, 2));
  res <- func.ert(x=data, goal.min=0);
  expect_identical(res, list(x=c(0,2), y=c(+Inf, 1L)));

  data <- list(c(1, 2));
  res <- func.ert(x=data, goal.max=10);
  expect_identical(res, list(x=c(2,10), y=c(1L, 1L)));

  data <- list(c(1, 2));
  res <- func.ert(x=data, goal.min=0, goal.max=10);
  expect_identical(res, list(x=c(0, 2,10), y=c(+Inf, 1L, 1L)));

  data <- list(matrix(c(1, 2, 2, 1), ncol=2));
  res <- func.ert(x=data);
  expect_identical(res, list(x=c(1, 2), y=c(2L, 1L)));

  data <- list(matrix(c(1, 2, 3, 2, 1, 0), ncol=2));
  res <- func.ert(x=data);
  expect_identical(res, list(x=c(0, 1, 2), y=c(3L, 2L, 1L)));

  data <- list(matrix(c(1, 2, 3, 2, 1, 0), ncol=2));
  res <- func.ert(x=data, goal.min=0);
  expect_identical(res, list(x=c(0, 1, 2), y=c(3L, 2L, 1L)));

  data <- list(matrix(c(1, 2, 3, 2, 1, 0), ncol=2));
  res <- func.ert(x=data, goal.min=-1);
  expect_identical(res, list(x=c(-1, 0, 1, 2), y=c(+Inf, 3L, 2L, 1L)));

  data <- list(matrix(c(1, 2, 3, 2, 1, 0), ncol=2));
  res <- func.ert(x=data, goal.max=10);
  expect_identical(res, list(x=c(0, 1, 2, 10), y=c(3L, 2L, 1L, 1L)));

  data <- list(matrix(c(1, 2, 3, 2, 1, 0), ncol=2));
  res <- func.ert(x=data, goal.min=-1, goal.max=10);
  expect_identical(res, list(x=c(-1, 0, 1, 2, 10), y=c(+Inf, 3L, 2L, 1L, 1L)));
})

test_that("Test func.ert (2)", {
  data <- list(c(1, 2), c(1, 2));
  res <- func.ert(x=data);
  expect_identical(res, list(x=c(2), y=c(1L)));

  data <- list(c(1, 2), c(1, 1));
  res <- func.ert(x=data);
  expect_identical(res, list(x=c(1, 2), y=c(2L, 1L)));

  data <- list(c(1, 1), c(1, 2));
  res <- func.ert(x=data);
  expect_identical(res, list(x=c(1, 2), y=c(2L, 1L)));

  data <- list(c(1, 1), c(1, 2));
  res <- func.ert(x=data, time.max=10);
  expect_identical(res, list(x=c(1, 2), y=c(11L, 1L)));

  data <- list(matrix(c(1, 2, 3, 2, 1, 0), ncol=2), c(1, 1));
  res <- func.ert(x=data);
  expect_identical(res, list(x=c(0, 1, 2), y=c(4L, 3/2, 1L)));

  data <- list(matrix(c(1, 2, 3, 2, 1, 0), ncol=2), c(1, 1));
  res <- func.ert(x=data, time.max=10);
  expect_identical(res, list(x=c(0, 1, 2), y=c(13L, 3/2, 1L)));
})


test_that("Test func.ert (3)", {
  data <- list(c(1, 2), c(1, 2), c(1, 2));
  res <- func.ert(x=data);
  expect_identical(res, list(x=c(2), y=c(1L)));

  data <- list(c(1, 2), c(1, 2), c(3, 2));
  res <- func.ert(x=data);
  expect_identical(res, list(x=c(2), y=c(5/3)));

  data <- list(c(1, 2), c(5, 2), c(3, 2));
  res <- func.ert(x=data);
  expect_identical(res, list(x=c(2), y=c(3L)));

  data <- list(c(1, 2), c(5, 2), c(3, 2));
  res <- func.ert(x=data, time.max=10);
  expect_identical(res, list(x=c(2), y=c(3L)));

  data <- list(c(1, 7), c(5, 1), c(3, 2));
  res <- func.ert(x=data);
  expect_identical(res, list(x=c(1, 2, 7), y=c(9, 4.5, 3L)));

  data <- list(c(1, 7), c(5, 1), c(3, 2));
  res <- func.ert(x=data, time.max=10);
  expect_identical(res, list(x=c(1, 2, 7), y=c(25L, 9L, 3L)));
})


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


test_that("Test func.ert random runs", {
  res <- func.ert(make.runs(1, 0));
  expect_identical(length(res$x), length(res$y));
  expect_gt(res$x[1L], 0);
  expect_true(all(is.finite(res$x)));
  expect_true(all(is.finite(res$y)));

  res <- func.ert(make.runs(1, 0), goal.min=0);
  expect_identical(length(res$x), length(res$y));
  expect_lte(res$x[1L], 0);
  expect_identical(res$y[1L], Inf);

  res <- func.ert(make.runs(1, 1));
  expect_identical(length(res$x), length(res$y));
  expect_lte(res$x[1L], 0);
  expect_true(all(is.finite(res$x)));
  expect_true(all(is.finite(res$y)));

  res <- func.ert(make.runs(2, 0));
  expect_identical(length(res$x), length(res$y));
  expect_gt(res$x[1L], 0);
  expect_true(all(is.finite(res$x)));
  expect_true(all(is.finite(res$y)));

  res <- func.ert(make.runs(2, 1));
  expect_identical(length(res$x), length(res$y));
  expect_lte(res$x[1L], 0);
  expect_true(all(is.finite(res$x)));
  expect_true(all(is.finite(res$y)));

  res <- func.ert(make.runs(2, 2));
  expect_identical(length(res$x), length(res$y));
  expect_lte(res$x[1L], 0);
  expect_true(all(is.finite(res$x)));
  expect_true(all(is.finite(res$y)));

  for(n in 1L:10L) {
    for(m in 0L:n) {
      res <- func.ert(make.runs(n, m));
      if(m > 0L) {
        expect_lte(res$x[1L], 0);
      } else {
        expect_gt(res$x[1L], 0);
      }
      expect_true(all(is.finite(res$x)));
      expect_true(all(is.finite(res$y)));
    }
  }
})
