
# the default modeler
#' @importFrom fields Krig
.model <- function(data)
              suppressWarnings(Krig(as.matrix(cbind(data$x, data$y)), data$z))

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
#' @param model a \code{function(data)} receiving one item from the \code{data}
#'   list, which i sued for modeling the data, or \code{NULL}, by default a
#'   Kriging-based modeling if no modeling is necessary
#' @param predict a \code{function(model, x, y)} for predicting \code{z}
#'   coordinates based on models, or \code{NULL} if no modeling is necessary; by
#'   default a Kriging-based predictor
#' @param modelSteps the number of steps for modeling along each axis
#' @param legend the legend
#' @param legendWidth the fraction of the plot to be allocated for the legend
#' @inheritDotParams scatterplot3d::scatterplot3d -x -y -z -type
#' @export batchPlot.3d
#' @importFrom scatterplot3d scatterplot3d
#' @importFrom graphics layout legend par plot.new
#' @include distinctColors.R
#' @include distinctSymbols.R
#' @example examples/batchPlot3d.R
batchPlot.3d <- function(data,
                    plotPoints=TRUE,
                    model=.model,
                    predict=.predict,
                    modelSteps=20,
                    legend=NULL,
                    legendWidth=0.3,
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
           widths=c(1 - legendWidth, legendWidth));

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

  # setup model axes and compute the modes
  models  <- NULL;
  z.range <- NULL;
  if(!(is.null(model) || is.null(predict))) {
    # compute the x range
    model.x <- params$xlim;
    if(is.null(model.x)) { model.x <- range(x); }
    model.x <- seq.default(from=min(model.x), to=max(model.x), length.out=modelSteps);

    # compute the y range
    model.y <- params$ylim;
    if(is.null(model.y)) { model.y <- range(y); }
    model.y <- seq.default(from=min(model.y), to=max(model.y), length.out=modelSteps);

    # create all the models
    models <- lapply(X=data, FUN=model);

    # use the models to compute the axis ranges
    for(m in models) {
      # now we interpolate the model and paint it as lines, first along x
      for(j in length(model.x):1L) {
        xx <- rep(model.x[j], length(model.y));
        z.range <- range(z.range, predict(m, xx, model.y));
      }

      # then along y
      for(j in length(model.y):1L) {
        yy <- rep(model.y[j], length(model.x));
        z.range <- range(z.range, predict(m, model.x, yy));
      }
    }
  }

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

    if(!(is.null(z.range))) {
      # we can only get here if we plot both models and points
      if(is.null(params$zlim)) {
        # if the user did not specify axis limits
        z.r.2 <- range(z); # then we compute the range of the z data
        if((z.r.2[1L] > z.range[1L]) || (z.r.2[2L] < z.range[2L])) {
          # if the models have a range which at some point extends to outside
          # the data range, we set the zlimit
          params$zlim <- range(z.range, z.r.2);
        }
      }
    }

    params$type <- "p";
  } else {
    # create empty plot
    params$type <- "n";
    params$x <- range(model.x);
    params$y <- range(model.y);

    if(is.null(z.range)) {
      z.range <- range(z);
    }
    params$z <- z.range;
  }

  if(is.null(params$xlab)) { params$xlab <- ""; }
  if(is.null(params$ylab)) { params$ylab <- ""; }
  if(is.null(params$zlab)) { params$zlab <- ""; }

  x <- NULL; y <- NULL; z <- NULL;

  # plot
  s3d <- do.call(scatterplot3d, params);

  if(!(is.null(models))) {
    # plot the actual models

    # iterate over the lines
    for(i in seq_along(models)) {
      m <- models[[i]];

      # now we interpolate the model and paint it as lines, first along x
      for(j in length(model.x):1L) {
        xx <- rep(model.x[j], length(model.y));
        s3d$points3d(xx, model.y, predict(m, xx, model.y),
                     type="l", col=color[i])
      }

      # then along y
      for(j in length(model.y):1L) {
        yy <- rep(model.y[j], length(model.x));
        s3d$points3d(model.x, yy, predict(m, model.x, yy),
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
