library("plotteR")
library("grDevices")
context("batchPlot.3d")

test_that("Test batch-plotting of 3d", {

  # create destination file
  dest <- tempfile(pattern="plot-test", fileext=".pdf");
  expect_false(file.exists(dest));

  pdf(file=dest, width=16, height=16);
  # set a random seed for replicability
  set.seed(1000L);

  # generate the first example dataset
  f1 <- function(x, y) (x*x - y*y);
  x1 <- runif(n=400, min=-2, max=2);
  y1 <- runif(n=400, min=-2, max=2);
  z1 <- f1(x1, y1);
  d1 <- list(x=x1, y=y1, z=z1);

  # generate the second example dataset
  f2 <- function(x, y) 0.8*x + 0.25*y - 7;
  x2 <- runif(n=400, min=-2, max=2);
  y2 <- runif(n=400, min=-2, max=2);
  # here we even add a bit of randomness into the data
  z2 <- rnorm(n=400, mean=f2(x2, y2), sd=0.3);
  d2 <- list(x=x2, y=y2, z=z2);

  # plot the data together with the interpolated surfaces
  batchPlot.3d(list(d1, d2), plotPoints=TRUE, legend=c("f1", "f2"), legendWidth=0.1);

  dev.off();
  expect_true(file.exists(dest));
  expect_gt(file.size(dest), 100L);
  unlink(dest);
})
