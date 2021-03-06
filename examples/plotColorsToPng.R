library(grDevices)

if(!exists("x.range")) x.range <- 4L;
if(!exists("y.range")) y.range <- 5L;

png(filename = "examples/plotColors.png",
    width = 640, height = as.integer(ceiling(640*(y.range/x.range))),
    units = "px", pointsize = 12,
    bg = "white")
source("examples/plotColors.R");
dev.off();
