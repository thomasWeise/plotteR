library(grDevices)

png(filename = "examples/printColors.png",
    width = 640, height = 640, units = "px", pointsize = 12,
    bg = "white")
source("examples/printColors.R");
dev.off();
