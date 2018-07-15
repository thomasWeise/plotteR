library("plotteR")
library("grDevices")
context("plot.colors")

test_that("Test plotting the colors", {

  graphics.off();

  # create destination file
  dest <- tempfile(pattern="plot-test", fileext=".pdf");
  expect_false(file.exists(dest));

  pdf(file=dest, width=6, height=6);

  # set the random seed for reproducibility
  set.seed(129855L);

  x.range <- 4L;
  y.range <- 5L;

  # set the number of diagrams appropriately
  old.par <- par(mfrow=c(y.range, x.range), mai=c(0.02, 0.02, 0.02, 0.02));

  # paint the diagrams
  n <- 1L;
  for(y in 1L:y.range) {
    for(x in 1L:x.range) {
      plot.colors(colors.distinct(n));
      n <- n + 1L;
    }
  }

  # reset the parameters
  invisible(par(old.par));

  dev.off();
  graphics.off();

  expect_true(file.exists(dest));
  expect_gt(file.size(dest), 100L);

  # delete temp file
  unlink(dest);
})
