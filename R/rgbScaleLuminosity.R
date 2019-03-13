#' @title Scale the Luminosity of a Color
#' @description The luminosity of a color is scaled in a range of \code{[-1,1]},
#'   where -1 means black, 1 means white, and 0 means the color at its current
#'   luminosity. A value in \code{(0, 1]} will make the color brighter while
#'   trying to preserve its chromotographical properties. A value in \code{[-1,
#'   0)} will make the color darker while trying to preserve its
#'   chromotographical properties. See
#'   \url{https://stackoverflow.com/questions/6615002}
#' @param r the red value
#' @param g the green value
#' @param b the blue value
#' @param scale the scale value, in \code{[-1,1]}
#' @param limit the maximum permitted value for each rgb-coordinate: defaults to
#'   255
#' @return a color with similar chromotographical properties but either lighter
#'   (scale>0) or darker (scale<0) complexion.
#' @export rgb.scale.luminosity
rgb.scale.luminosity <- function(r, g, b, scale, limit=255) {
  if(scale >= 1) { return(c(limit, limit, limit)); }
  if(scale <= (-1)) { return(c(0, 0, 0)); }
  if(scale == 0) { return(c(r, g, b)); }

  cc <- (c(r, g, b) / limit) ^ 2.2;

  if(scale < 0) {
    m <- (1 + scale);
    cc <- m*cc;
  } else {
    cc <- cc + (scale * (1 - cc));
  }

  return((cc ^ (1/2.2)) * limit);
}
