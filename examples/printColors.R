library(plotteR)

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
