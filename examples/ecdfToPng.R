library(grDevices)

png(filename = "examples/ecdf.png",
    width = 640L, height = 480L,
    units = "px", pointsize = 12,
    bg = "white")
source("examples/ecdf.R");
dev.off();
