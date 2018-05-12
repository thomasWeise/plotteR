library("plotteR")
library("grDevices")
context("batchPlot.groups")

test_that("Test batch-plotting of groups", {

  # create destination file
  dest <- tempfile(pattern="plot-test", fileext=".pdf");
  expect_false(file.exists(dest));

  pdf(dest, width=6, height=3);

  # the three base functions with the mean parameter values
  f <- list(
    list(f=function(x, par) par[1] + par[2]*x + par[3]*x*x + par[4]*x*x*x,
         m=c(1, -0.2, 0.75, -0.3)),
    list(f = function(x, par) par[1] * exp(par[2] - x),
         m=c(0.1, 3)),
    list(f=function(x, par) par[1] + par[2]*sin(par[3]*x),
         m=c(0, 1, 3)));

  # create the three example data sets
  examples <- lapply(X=f, FUN=function(example) {
    # for each example function, plot 4 to 50 instances
    lapply(seq_len(runif(n=1, min=4, max=50)),
           # for each instance
           FUN=function(i) {
             # randomly choose the x-coordinates
             x <- runif(n=as.integer(round(runif(n=1, min=10, max=200))),
                        min=0, max=3);
             m <- example$m;
             # pick parameters which are normally distributed around the suggestion
             par <- rnorm(n=length(m), mean=m, s=0.1*abs(m));
             # and construct a function
             fff <- function(x) example$f(x, par);
             # and pass this function as result example together with the x values
             list(x=x, f=fff)
           })
  });

  # plot the original data
  batchPlot.groups(examples,
                   names=c("f1", "f2", "f3"),
                   ffun = function(l, x) l$f(x),
                   main="Original Data and Function Values for x",
                   plotXY=FALSE, plotXF=TRUE,
                   legend=list(x="bottom", horiz=TRUE));

  dev.off();

  expect_true(file.exists(dest));
  expect_gt(file.size(dest), 100L);

  # delete temp file
  unlink(dest);
})
