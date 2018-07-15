# generate the first example dataset
f1 <- function(x, y) sin(x*x - y);
x1 <- rnorm(n=100, mean=2, sd=2);
y1 <- rnorm(n=100, mean=-1, sd=3);
z1 <- f1(x1, y1);
d1 <- list(x=x1, y=y1, z=z1);

# generate the second example dataset
f2 <- function(x, y) exp((x - 0.5*y)^2);
x2 <- runif(n=200, min=-2, max=2);
y2 <- runif(n=200, min=-2, max=2);
z2 <- f2(x2, y2);
d2 <- list(x=x2, y=y2, z=z2);

# plot
batchPlot.3d(list(d1, d2));
