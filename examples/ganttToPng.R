library(grDevices)

png(filename = "examples/gantt.png",
    width = 640L, height = 480L,
    units = "px", pointsize = 12,
    bg = "white")
source("examples/gantt.R");
dev.off();
