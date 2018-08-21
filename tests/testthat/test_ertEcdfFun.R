library("plotteR")
context("func.ertEcdf")

test_that("Test func.ertEcdf (1)", {
  data <- list(list(c(1, 2)));
  res <- func.ertEcdf(x=data, goal=1);
  expect_equal(res, list(x=c(1), y=c(0)));

  data <- list(list(c(1, 2)));
  res <- func.ertEcdf(x=data, goal=2);
  expect_equal(res, list(x=c(1), y=c(1)));


  data <- list(list(c(1, 2), c(1, 2)));
  res <- func.ertEcdf(x=data, goal=1);
  expect_equal(res, list(x=c(1), y=c(0)));
  data <- list(list(c(1, 2), c(2, 2)));
  res <- func.ertEcdf(x=data, goal=1);
  expect_equal(res, list(x=c(1), y=c(0)));


  data <- list(list(c(1, 2), c(1, 2)));
  res <- func.ertEcdf(x=data, goal=2);
  expect_equal(res, list(x=c(1), y=c(1)));

  data <- list(list(c(1, 2), c(2, 2)));
  res <- func.ertEcdf(x=data, goal=2);
  expect_equal(res, list(x=c(1, 3/2), y=c(0, 1)));


  data <- list(list(c(1, 2), c(2, 2)), list(c(1, 2)));
  res <- func.ertEcdf(x=data, goal=2);
  expect_equal(res, list(x=c(1, 3/2), y=c(0.5, 1)));


  data <- list(list(c(1, 2), c(2, 2)), list(c(1, 2)), list(c(3, 2)));
  res <- func.ertEcdf(x=data, goal=2);
  expect_equal(res, list(x=c(1, 3/2, 3), y=c(1/3, 2/3, 3/3)));


  data <- list(list(c(1, 2), c(2, 2)), list(c(1, 2)), list(c(3, 2)), list(c(6,1), c(5,1)));
  res <- func.ertEcdf(x=data, goal=2);
  expect_equal(res, list(x=c(1, 3/2, 3, 11/2), y=c(1/4, 2/4, 3/4, 4/4)));
})
