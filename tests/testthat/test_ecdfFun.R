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
