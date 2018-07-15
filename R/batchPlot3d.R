
# the default modeler
#' @importFrom fields Krig
.model <- function(x, y, z)
              suppressWarnings(Krig(as.matrix(cbind(x, y)), z))

# the default predictor
#' @importFrom fields predict.Krig
.predict <- function(model, x, y)
                suppressWarnings(predict.Krig(model, as.matrix(cbind(x, y))))

#' @title Plot 3D Data
#' @description The list \code{x} contains, in turn,
#' lists which have the elements \code{x}, \code{y}, \code{z},
#' each of which represent one dataset to be plotted.
#' @param data the data list, a list of lists where each element
#' is a list with elements \code{x}, \code{y}, \code{z}, which must be vectors.
#' @param plotPoints should the raw points be plotted?
#' @param model a \code{function(x, y, z)} for modeling the data, or
#'   \code{NULL}, by default a Kriging-based modeling if no modeling is
#'   necessary
#' @param predict a \code{function(model, x, y)} for predicting \code{z}
#'   coordinates based on models, or \code{NULL} if no modeling is necessary; by
#'   default a Kriging-based predictor
#' @param modelSteps the number of steps for modeling along each axis
#' @param legend the legend
#' @inheritDotParams scatterplot3d::scatterplot3d -x -y
#' @export batchPlot.3d
#' @importFrom scatterplot3d scatterplot3d
#' @importFrom graphics layout legend par plot.new
#' @include distinctColors.R
#' @include distinctSymbols.R
batchPlot.3d <- function(data,
                    plotPoints=TRUE,
                    model=.model,
                    predict=.predict,
                    modelSteps=25,
                    legend=NULL,
                    ...) {

  # load the parameters
  params <- list(...);

  # store the current graphical setup
  old.par <- par();
  # remove parameters which would cause warnings
  old.par <- old.par[names(old.par) != "cin"];
  old.par <- old.par[names(old.par) != "cra"];
  old.par <- old.par[names(old.par) != "csi"];
  old.par <- old.par[names(old.par) != "cxy"];
  old.par <- old.par[names(old.par) != "din"];
  old.par <- old.par[names(old.par) != "page"];

  if(!(is.null(legend))) {
    stopifnot(length(legend) == length(data));
    # divide the pane horizontally into two, so we have space for the legend
    layout(matrix(c(1L, 2L), nrow=1L, ncol=2L),
           widths=c(0.7, 0.3));

    # set margins: no margin on the right side
    params$mar <- c(5.1, 3.1, 4.1, 0);
  }

  # setup the colors
  color <- params$color;
  if(is.null(color)) { color <- colors.distinct(length(data)); }

  # setup the symbols
  symbols <- params$pch;
  if(is.null(symbols)) { symbols <- symbols.distinct(length(data)); }

  x <- unlist(lapply(X=data, FUN=function(d) d$x), recursive=TRUE);
  y <- unlist(lapply(X=data, FUN=function(d) d$y), recursive=TRUE);
  z <- unlist(lapply(X=data, FUN=function(d) d$z), recursive=TRUE);

  stopifnot(length(x) == length(y), length(y) == length(z));

  if(plotPoints) {
    # setup colors
    params$color <- unlist(lapply(X=seq_along(data),
                        FUN=function(i) rep(color[i],
                                            length(data[[i]]$x))),
                        recursive = TRUE);
    # setup symbols
    params$pch   <- unlist(lapply(X=seq_along(data),
                                  FUN=function(i) rep(symbols[i],
                                                      length(data[[i]]$x))),
                           recursive = TRUE);
    params$x <- x;
    params$y <- y;
    params$z <- z;
  } else {
    # create empty plot
    params$type <- "n";
    params$x <- range(x);
    params$y <- range(y);
    params$z <- range(z);
  }

  if(is.null(params$xlab)) { params$xlab <- ""; }
  if(is.null(params$ylab)) { params$ylab <- ""; }
  if(is.null(params$zlab)) { params$zlab <- ""; }

  x <- NULL; y <- NULL; z <- NULL;

  # plot
  s3d <- do.call(scatterplot3d, params);

  if(!(is.null(model) || is.null(predict))) {
    # compute the x range
    x <- params$xlim;
    if(is.null(x)) { x <- range(params$x); }
    x <- seq.default(from=min(x), to=max(x), length.out=modelSteps);

    # compute the y range
    y <- params$ylim;
    if(is.null(y)) { y <- range(params$y); }
    y <- seq.default(from=min(y), to=max(y), length.out=modelSteps);

    # iterate over the lines
    for(i in seq_along(data)) {
      # select the ith dataset
      d <- data[[i]];

      # model it
      m <- model(d$x, d$y, d$z);

      # now we interpolate the model and paint it as lines, first along x
      for(j in length(x):1L) {
        xx <- rep(x[j], length(y));
        s3d$points3d(xx, y, predict(m, xx, y),
                     type="l", col=color[i])
      }

      # then along y
      for(j in length(y):1L) {
        yy <- rep(y[j], length(x));
        s3d$points3d(x, yy, predict(m, x, yy),
                     type="l", col=color[i])
      }
    }
  }

  if(!(is.null(legend))) {
    # paint the legend
    # move to next graphic, the one where we will put the legend
    plot.new();

    n.par <- old.par;
    n.par$mar <- 0.3*old.par$mar;
    par(n.par);

    # setup the parameters for the legend
    lparams <- list();
    lparams$x <- "right";
    lparams$legend <- legend;
    lparams$col <- color;
    lparams$text.col <- color;
    lparams$bty <- "n";

    if(plotPoints) { # put symbols only if points are shown
      lparams$pch <- symbols;
    }

    # paint the legend
    do.call(graphics::legend, lparams);
  }


  # restore graphical parameters
  par(old.par);
}
