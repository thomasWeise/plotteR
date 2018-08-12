library("plotteR")
context("func.ecdf")

test_that("Test func.ecdf no success", {
  data <- list(c(1, 2));
  res <- func.ecdf(x=data, goal=0);
  expect_identical(res, list(x=c(1L), y=c(0)));

  data <- list(c(1, 2), c(2, 2));
  res <- func.ecdf(x=data, goal=0);
  expect_identical(res, list(x=c(1L, 2L), y=c(0, 0)));

  data <- list(c(1, 2), c(3, 4), matrix(c(1, 2, 3, 1), ncol=2));
  res <- func.ecdf(x=data, goal=0);
  expect_identical(res, list(x=c(1L, 3L), y=c(0, 0)));

  data <- list(c(1, 2), c(3, 4), matrix(c(1, 3, 3, 1), ncol=2));
  res <- func.ecdf(x=data, goal=0);
  expect_identical(res, list(x=c(1L, 3L), y=c(0, 0)));

  data <- list(matrix(c(1, 2, 3, 1), ncol=2));
  res <- func.ecdf(x=data, goal=0);
  expect_identical(res, list(x=c(1L, 2L), y=c(0, 0)));

  data <- list(matrix(c(1, 3, 3, 1), ncol=2));
  res <- func.ecdf(x=data, goal=0);
  expect_identical(res, list(x=c(1L, 3L), y=c(0, 0)));
})


test_that("Test func.ecdf full success", {
  data <- list(c(1, 0));
  res <- func.ecdf(x=data, goal=0);
  expect_identical(res, list(x=c(1L), y=c(1)));

  data <- list(c(2, 0));
  res <- func.ecdf(x=data, goal=0);
  expect_identical(res, list(x=c(1L, 2L), y=c(0, 1)));

  data <- list(c(1, 3));
  res <- func.ecdf(x=data, goal=3);
  expect_identical(res, list(x=c(1L), y=c(1)));


  data <- list(c(4, 0));
  res <- func.ecdf(x=data, goal=0);
  expect_identical(res, list(x=c(1L, 4L), y=c(0, 1)));
})

test_that("Test func.ecdf partial success", {
  data <- list(c(4, 0), c(5, 0));
  res <- func.ecdf(x=data, goal=0);
  expect_identical(res, list(x=c(1L, 4L, 5L), y=c(0, 0.5, 1)));

  data <- list(c(4, 0), c(5, 0), c(6, 1), c(7, 2), c(4, 0), c(5, -1));
  res <- func.ecdf(x=data, goal=0);
  expect_identical(res, list(x=c(1L, 4L, 5L, 7L), y=c(0, 1/3, 2/3, 2/3)));

  data <- list(c(4, 0), c(5, 0), c(6, 1), c(7, 2), c(4, 0), c(5, -1));
  res <- func.ecdf(x=data, goal=0, time.max=20);
  expect_identical(res, list(x=c(1L, 4L, 5L, 20L), y=c(0, 1/3, 2/3, 2/3)));

  data <- list(c(4, 0), c(5, 0), c(6, 1), c(7, 2), c(4, 0), c(5, -1));
  res <- func.ecdf(x=data, goal=0, time.max=5);
  expect_identical(res, list(x=c(1L, 4L, 5L), y=c(0, 1/3, 2/3)));

  data <- list(matrix(c(1,2,3,4,4,3,0,-1), ncol=2));
  res <- func.ecdf(x=data, goal=0);
  expect_identical(res, list(x=c(1L, 3L, 4L), y=c(0, 1, 1)));

  data <- list(matrix(c(1,2,3,4,4,3,0,-1), ncol=2), matrix(c(1,4,6,8,4,3,0,-1), ncol=2));
  res <- func.ecdf(x=data, goal=0);
  expect_identical(res, list(x=c(1L, 3L,6L, 8L), y=c(0, 0.5, 1, 1)));
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



test_that("Test func.ecdf random runs", {
  res <- func.ecdf(make.runs(1, 0));
  expect_length(res$x, 2L);
  expect_identical(res$y, c(0, 0));

  res <- func.ecdf(make.runs(1, 1));
  expect_gte(length(res$x), 2L);
  expect_identical(res$y[2L], 1);
  expect_identical(res$y[length(res$y)], 1);

  res <- func.ecdf(make.runs(2, 0));
  expect_length(res$x, 2L);
  expect_identical(res$y, c(0, 0));

  res <- func.ecdf(make.runs(2, 1));
  expect_gte(length(res$x), 2L);
  expect_identical(res$y[2L], 0.5);
  expect_identical(res$y[length(res$y)], 0.5);

  res <- func.ecdf(make.runs(2, 2));
  expect_gte(length(res$x), 2L);
  expect_identical(res$y[length(res$y)], 1);

  res <- func.ecdf(make.runs(3, 0));
  expect_length(res$x, 2L);
  expect_identical(res$y, c(0, 0));

  res <- func.ecdf(make.runs(3, 1));
  expect_gte(length(res$x), 2L);
  expect_identical(res$y[2L], 1/3);
  expect_identical(res$y[length(res$y)], 1/3);

  res <- func.ecdf(make.runs(3, 2));
  expect_gte(length(res$x), 2L);
  expect_identical(res$y[length(res$y)], 2/3);

  res <- func.ecdf(make.runs(3, 3));
  expect_gte(length(res$x), 2L);
  expect_identical(res$y[length(res$y)], 1);

  for(n in 1L:10L) {
    for(m in 0L:n) {
      res <- func.ecdf(make.runs(n, m));
      expect_gte(length(res$x), 2L);
      expect_identical(length(res$x), length(res$y));
      expect_identical(res$y[length(res$y)], m/n);
    }
  }
})
