library(plotteR)

# set a random seed for replicability
set.seed(1367);

# make an example
make.example <- function(f) {
  suppressWarnings({
    repeat {
      n <- as.integer(round(runif(n=1, min=10, max=20)));
      x <- sort(runif(n=n, min=-5, max=5)); # generate x data
      y <- rnorm(n=n, mean=f(x), s=0.1);  # noisy y
      x <- rnorm(n=n, mean=x, s=0.1); # noisy x
      fi <- is.finite(x) & is.finite(y);
      x <- x[fi];
      if(length(x) > 4L) {
        y <- y[fi];
        return(list(x=x, y=y, f=f));
      }
    }
  });
}

# the three base functions
f <- c(function(x) 1 - 0.2*x + 0.75*x*x - 0.3*x*x*x,
       log,   # this function becomes non-finite for x <= 0
       asin); # non-finite for any x outside of [-1, 1]

# create the three example data sets
examples <- lapply(X=f, FUN=make.example);

old <- par(mfcol=c(2, 1));

# plot the original data, which only covers part of the x.axis
batchPlot.list(examples,
               names=c("f1", "f2", "f3"),
               main="Original Data and Function Values for x",
               legend=list(x="left"),
               y.min.lower = -3,
               y.max.upper= 3);
abline(v=-1, col="gray");abline(v=0, col="gray");abline(v=1, col="gray");


# extending the function lines towards their smallest and largest finite
# points
batchPlot.list(examples,
               names=c("f1", "f2", "f3"),
               main="Functions Extented by Finite Point Search",
               legend=list(x="left"),
               y.min.lower = -3,
               y.max.upper= 3,
               x.add = TRUE);
abline(v=-1, col="gray");abline(v=0, col="gray");abline(v=1, col="gray");

par(old);
