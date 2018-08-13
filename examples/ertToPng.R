library(grDevices)

png(filename = "examples/ert.png",
    width = 640L, height = 480L,
    units = "px", pointsize = 12,
    bg = "white")
source("examples/ert.R");
dev.off();
