library(grDevices)

png(filename = "examples/batchPlotList.png",
    width = 640, height = 480, units = "px", pointsize = 12,
    bg = "white")
source("examples/batchPlotList.R");
dev.off();
