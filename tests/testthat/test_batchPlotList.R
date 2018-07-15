library("plotteR")
library("grDevices")
context("batchPlot.list")

test_that("Test batch-plotting of lists 1", {

  graphics.off();

  # create destination file
  dest <- tempfile(pattern="plot-test", fileext=".pdf");
  expect_false(file.exists(dest));

  pdf(file=dest, width=6, height=3);

  source("../../examples/batchPlotList.R");

  dev.off();
  graphics.off();

  expect_true(file.exists(dest));
  expect_gt(file.size(dest), 100L);

  # delete temp file
  unlink(dest);
})
