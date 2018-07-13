library(plotteR)

# set the random seed for reproducibility
set.seed(129855L);

# choose the number of rows/columns for the diagram
root = 5L;

# set the number of diagrams appropriately
old.par <- par(mfrow=c(root, root), mai=c(0.02, 0.02, 0.02, 0.02));

# paint the diagrams
for(n in seq_len(root*root)) {
  plot.colors(colors.distinct(n));
}

# reset the parameters
invisible(par(old.par));
