#' @title Convert a RGB Color to Gray Scale using Luminosity Weights
#' @description We apply the simple algorithm from
#'   \url{http://www.johndcook.com/blog/2009/08/24/algorithms-convert-color-grayscale/}
#'   to convert rgb values to gray-scale based on luminosity. Notice that this
#'   function does not care about the range of \code{r}, \code{g}, and \code{b}:
#'   If all of them are in \code{0..255}, then the output will also be in
#'   \code{[0,255]} (though not necessary integer). If they are in \code{[0,1]},
#'   the output is also in \code{[0,1]}. As coefficients, we use the values for
#'   BT.709 (\url{http://en.wikipedia.org/wiki/Rec._709}).
#' @param r the red value
#' @param g the green value
#' @param b the blue value
#' @return  the gray scale value.
#' @export rgb2gray.luminosity
rgb2gray.luminosity <- function(r, g, b) {
  if((r == g) && (g == b)) { return(r); }
  return((0.2126*r) + (0.7152*g) + (0.0722*b));
}
