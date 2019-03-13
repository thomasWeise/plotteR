library(plotteR)

# create the colors
colors <- unique(unlist(c("red", "green", "blue", "cyan", "yellow", "pink",
                          "lightblue", "orange", "violet", "brown", "magenta",
                          "lightgreen",
                           unlist(lapply(3L:20L, colors.distinct)))));
# transform the colors to RGB and keep only unique colors
colors <- unique(lapply(colors, function(cc) as.integer(round(as.vector(unname(col2rgb(cc))[1L:3L]))) ));
colors <- colors[!duplicated(lapply(colors, function(cc) as.integer(cc/10)))];
colors <- colors[order(round(vapply(colors, function(x) rgb2gray.luminosity(x[1L], x[2L], x[3L]), 0)),
                       round(vapply(colors, function(x) x[2L], 0)),
                       round(vapply(colors, function(x) x[1L], 0)),
                       round(vapply(colors, function(x) x[3L], 0)))];

# the color range now is from 0 to 255
limit <- 255L;

# define the ranges for the coordinates
x.min <- -1;
x.max <- 1;
y.min <- 0;
y.max <- length(colors);


# set the number of diagrams appropriately
old.par <- par(mai=c(0, 0, 0, 0));

# set an empty plot
plot(type="n",
     x=c(x.min, x.max),
     xlim=c(x.min, x.max),
     y=c(y.min, y.max),
     ylim=c(y.min, y.max),
     xaxt="n",
     yaxt="n",
     xlab=NA,
     ylab=NA,
     main=NA,
     sub=NA,
     bty="n");

# compute coordinates for rectangles
y.coords <- seq.int(from=y.min, to=y.max, length.out = (length(colors)+1));
x.values <- seq.int(from=x.min, to=x.max, length.out = (2L*255L)+1L);
x.coords <- seq.int(from=x.min, to=x.max, length.out = length(x.values)+1L)

# iterate over colors
for(i in seq_along(colors)) {
  color <- colors[[i]];
  c.y.min <- y.coords[i];
  c.y.max <- y.coords[i+1L];
# iterate over modifications
  for(j in seq_along(x.values)) {
    c.x.min <- x.coords[j];
    c.x.max <- x.coords[j+1L];
    scale   <- x.values[j];
    col     <- rgb.scale.luminosity(color[1L], color[2L], color[3L], scale, limit=limit);
    rect(xleft=c.x.min, ybottom=c.y.min, xright=c.x.max, ytop=c.y.max,
         col=rgb(col[1L], col[2L], col[3L], maxColorValue = limit),
         lwd=0,  border=NA);
  }
}

# draw the center line
abline(v=0, col="black", lwd=2);

par(old.par);
