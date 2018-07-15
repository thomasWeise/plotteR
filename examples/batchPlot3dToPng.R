library(grDevices)

old.readline <- readline;
readline <- function(promt) {
  dev.off();
  png(filename = "examples/batchPlot3d_2.png",
      width = 640, height = 480,
      units = "px", pointsize = 12,
      bg = "white")
}

png(filename = "examples/batchPlot3d_1.png",
    width = 640, height = 480,
    units = "px", pointsize = 12,
    bg = "white")
source("examples/batchPlot3d.R");
dev.off();

readline <- old.readline;
