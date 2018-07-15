library("plotteR")
library("grDevices")
context("plot.colors")

test_that("Test plotting the colors", {

  graphics.off();

  # create destination file
  dest <- tempfile(pattern="plot-test", fileext=".pdf");
  expect_false(file.exists(dest));

  pdf(file=dest, width=6, height=6);

  x.range <- 4L;
  y.range <- 4L;

  source("../../examples/plotColors.R", local=environment());

  dev.off();
  graphics.off();

  expect_true(file.exists(dest));
  expect_gt(file.size(dest), 100L);

  # delete temp file
  unlink(dest);
})
