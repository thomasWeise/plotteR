library(plotteR)

# set the random seed for reproducibility
set.seed(129855L);

# choose the number of rows/columns for the diagram
root = 6L;

# set the number of diagrams appropriately
old.par <- par(mfrow=c(root, root), mai=c(0.02, 0.02, 0.02, 0.02));

# paint the diagrams
for(i in seq_len(root*root)) {
  colors <- colors.distinct(i);
  pie(x=rep(1, i), labels=NA, col=colors,
      radius=1, border=FALSE, lty=0);
}

# reset the parameters
invisible(par(old.par));
