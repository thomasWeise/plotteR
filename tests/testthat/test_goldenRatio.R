library("plotteR")
library("grDevices")
context("golden ratio")

test_that("Test golden ratio value", {
  expect_gt(plotteR:::.goldenRatio, 1.6180330);
  expect_lt(plotteR:::.goldenRatio, 1.618034);
})

test_that("Test plot.pleasing.size", {
  r <- plots.pleasing.size();
  expect_true(is.list(r));
  expect_length(r, 2L);
  expect_true(is.numeric(r$width));
  expect_length(r$width, 1L);
  expect_gt(r$width, 0);
  expect_lt(r$width, 10);
  expect_true(is.numeric(r$height));
  expect_length(r$height, 1L);
  expect_gt(r$height, 0);
  expect_lt(r$height, 10);
  expect_lt(r$height, r$width);

  r <- plots.pleasing.size(portrait=TRUE);
  expect_true(is.list(r));
  expect_length(r, 2L);
  expect_true(is.numeric(r$width));
  expect_length(r$width, 1L);
  expect_gt(r$width, 0);
  expect_lt(r$width, 10);
  expect_true(is.numeric(r$height));
  expect_length(r$height, 1L);
  expect_gt(r$height, 0);
  expect_lt(r$height, 10);
  expect_gt(r$height, r$width);

  r <- plots.pleasing.size(width=3, portrait=TRUE);
  expect_true(is.list(r));
  expect_length(r, 2L);
  expect_true(is.numeric(r$width));
  expect_length(r$width, 1L);
  expect_gt(r$width, 0);
  expect_lt(r$width, 10);
  expect_true(is.numeric(r$height));
  expect_length(r$height, 1L);
  expect_gt(r$height, 0);
  expect_lt(r$height, 10);
  expect_gt(r$height, r$width);
  expect_identical(r$width, 3);

  r <- plots.pleasing.size(width=3, portrait=FALSE);
  expect_true(is.list(r));
  expect_length(r, 2L);
  expect_true(is.numeric(r$width));
  expect_length(r$width, 1L);
  expect_gt(r$width, 0);
  expect_lt(r$width, 10);
  expect_true(is.numeric(r$height));
  expect_length(r$height, 1L);
  expect_gt(r$height, 0);
  expect_lt(r$height, 10);
  expect_lt(r$height, r$width);
  expect_identical(r$width, 3);

  r <- plots.pleasing.size(height=3, portrait=TRUE);
  expect_true(is.list(r));
  expect_length(r, 2L);
  expect_true(is.numeric(r$width));
  expect_length(r$width, 1L);
  expect_gt(r$width, 0);
  expect_lt(r$width, 10);
  expect_true(is.numeric(r$height));
  expect_length(r$height, 1L);
  expect_gt(r$height, 0);
  expect_lt(r$height, 10);
  expect_gt(r$height, r$width);
  expect_identical(r$height, 3);

  r <- plots.pleasing.size(height=3, portrait=FALSE);
  expect_true(is.list(r));
  expect_length(r, 2L);
  expect_true(is.numeric(r$width));
  expect_length(r$width, 1L);
  expect_gt(r$width, 0);
  expect_lt(r$width, 10);
  expect_true(is.numeric(r$height));
  expect_length(r$height, 1L);
  expect_gt(r$height, 0);
  expect_lt(r$height, 10);
  expect_lt(r$height, r$width);
  expect_identical(r$height, 3);
})
