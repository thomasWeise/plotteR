library("plotteR")
context("distinctColors")

test_that("Test colors.distinct 1", {
  expect_length(colors.distinct(1), 1);
  expect_length(colors.distinct(2), 2);
  expect_length(colors.distinct(3), 3);
  expect_length(colors.distinct(4), 4);
})
