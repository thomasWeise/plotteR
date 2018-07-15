library("plotteR")
library("grDevices")
context("batchPlot.list")

test_that("Test batch-plotting of lists 1", {

  graphics.off();

  # create destination file
  dest <- tempfile(pattern="plot-test", fileext=".pdf");
  expect_false(file.exists(dest));

  pdf(file=dest, width=6, height=3);

  # set a random seed for replicability
  set.seed(1367);

  # make an example
  make.example <- function(f) {
    n <- as.integer(round(runif(n=1, min=10, max=200)));
    x <- sort(runif(n=n, min=0, max=3)); # generate x data
    y <- rnorm(n=n, mean=f(x), s=0.1);  # noisy y
    x <- rnorm(n=n, mean=x, s=0.1); # noisy x
    return(list(x=x, y=y, f=f));
  }

  # the three base functions
  f <- c(function(x) 1 - 0.2*x + 0.75*x*x - 0.3*x*x*x,
         function(x) 0.1 * exp(3 - x),
         function(x) 1.2 + 0.7*sin(2*x));

  # create the three example data sets
  examples <- lapply(X=f, FUN=make.example);

  # plot the original data
  batchPlot.list(examples,
                 names=c("f1", "f2", "f3"),
                 main="Original Data and Function Values for x",
                 legend=list(x="bottom", horiz=TRUE));


  dev.off();
  graphics.off();

  expect_true(file.exists(dest));
  expect_gt(file.size(dest), 100L);

  # delete temp file
  unlink(dest);
})
