# the golden ratio
.goldenRatio <- ((1 + sqrt(5)) / 2)

.paper.width  <- min(8.5, #letter
                     8.3); # a4
.paper.height <- min(11, #letter
                     11.7); # a4

.min.dim <- 1.3;

#' @title Compute a Pleasing Plot Size
#' @description Return a pleasing size for plots, based on the {@code width} and
#'   {@code height}, if provided.
#' @param width the optional width of the plot
#' @param height the optional height of the plot
#' @param paperWidthFrac optional, a fraction of the default paper width
#' @param paperHeightFrac optional, a fraction of the default paper height
#' @param portrait optional, should we use portrait orientation or letter
#'   orientation({@code FALSE})
#' @param boundToPaper should we bound the result dimensions to the paper
#'     size?
#' @return a \code{list(width, height)} of reasonable figure dimensions
#' @export plots.pleasing.size
#' @examples
#' plots.pleasing.size(width=3)
#' # $width=3, $height=1.854102
#' plots.pleasing.size(height=3)
#' # $width=4.854102, $height=3
#' plots.pleasing.size()
#' # $width=8.3, $height=5.129682
plots.pleasing.size <- function(width=NULL, height=NULL,
                                paperWidthFrac=NULL,
                                paperHeightFrac=NULL,
                                portrait=FALSE,
                                boundToPaper=TRUE) {

  forceWidth    <- (!(missing(width) && missing(paperWidthFrac)));
  forceHeight   <- (!(missing(height) && missing(paperHeightFrac)));
  forcePortrait <- (!(missing(portrait)));

  # check if we can setup the width from the parameters
  if(is.null(width)) {
    # width is not given, use frac if provided
    if(!(is.null(paperWidthFrac))) {
      width <- max(.min.dim, paperWidthFrac * .paper.width);
    }
  } else {
    # width is given, limit to frac if provided
    if(!(is.null(paperWidthFrac))) {
      width <- max(.min.dim, min(width,
                   paperWidthFrac*.paper.width));
    }
  }

  # check if we can setup the height from the parameters
  if(is.null(height)) {
    # height is not given, use frac if provided
    if(!(is.null(paperHeightFrac))) {
      height <- max(.min.dim, paperHeightFrac * .paper.height);
    }
  } else {
    # height is given, limit to frac if provided
    if(!(is.null(paperHeightFrac))) {
      height <- max(.min.dim, min(height,
                    paperHeightFrac * .paper.height));
    }
  }

  # fallback setting of dimensions
  if(is.null(width)) {
    # neither width nor width frac is given
    if(is.null(height)) {
      # also no height
      if(portrait) {
        # portrait: use 90% of full paper
        width  <- .paper.width;
        height <- 0.9*.paper.height;
      } else {
        # landscape: use full width, set height by golden ratio
        width  <- .paper.width;
        height <- max(.min.dim, width / .goldenRatio);
      }
    } else {
      # no width, but we have a height
      if(portrait) {
        # portrait orientation: wider than high
        width <- max(.min.dim, height / .goldenRatio);
      } else {
        # landscape orientation, compute width from height
        width <- height * .goldenRatio;
      }
    }
  } else {
    # width is given
    if(is.null(height)) {
      # height is missing
      if(portrait) {
        # landscape orientation: wider than high
        height <- width * .goldenRatio;
      } else {
        # portrait orientation, compute height from width
        height <- max(.min.dim, width / .goldenRatio);
      }
    }
  }

  # should we enforce portrait conditions?
  if(forcePortrait) {
    if((   portrait   && (width > height)) ||
       ((!(portrait)) && (width < height))) {
      s           <- width;
      width       <- height;
      height      <- s;
      s           <- forceWidth;
      forceWidth  <- forceHeight;
      forceHeight <- s;
    }
  }

  fit.1 <- .paper.width;
  fit.2 <- (0.9*.paper.height);

  # make width coordinates fit to paper
  if(boundToPaper) {
    minFracToFit <- 0.95;
    maxFracToFit <- +Inf;
  } else {
    if(forceWidth) {
      minFracToFit <- 0.95;
      maxFracToFit <- 1.05;
    } else {
      minFracToFit <- 0.85;
      maxFracToFit <- 1.25;
    }
  }

  # now fit them
  if((width >= (minFracToFit * fit.1)) &&
     (width <= (maxFracToFit * fit.1))) {
    width <- fit.1;
  } else {
    if((width >= (minFracToFit * fit.2)) &&
       (width <= (maxFracToFit * fit.2))) {
      width <- fit.2;
    }
  }

  # make height coordinates fit to paper
  if(boundToPaper) {
    minFracToFit <- 0.95;
    maxFracToFit <- +Inf;
  } else {
    if(forceHeight) {
      minFracToFit <- 0.95;
      maxFracToFit <- 1.05;
    } else {
      minFracToFit <- 0.85;
      maxFracToFit <- 1.25;
    }
  }

  # now fit them
  if((height >= (minFracToFit * fit.2)) &&
     (height <= (maxFracToFit * fit.2))) {
    height <- fit.2;
  } else {
    if((height >= (minFracToFit * fit.1)) &&
       (height <= (maxFracToFit * fit.1))) {
      height <- fit.1;
    }
  }

  # final check: should we enforce portrait conditions?
  if(forcePortrait) {
    if((   portrait   && (width > height)) ||
       ((!(portrait)) && (width < height))) {
      s           <- width;
      width       <- height;
      height      <- s;
      s           <- forceWidth;
      forceWidth  <- forceHeight;
      forceHeight <- s;
    }
  }

  return(list(width=width, height=height));
}
