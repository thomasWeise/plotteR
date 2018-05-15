# a set of pre-defined goal sizes for images, in c(width, height)
.a6.in <-  c( 5.8,  4.1);
.a5.in <-  c( 8.3,  5.8);
.a4.in <-  c(11.7,  8.3);
.a3.in <-  c(16.5, 11.7);
.a2.in <-  c(23.4, 16.5);
.a1.in <-  c(33.1, 23.4);
.a0.in <-  c(46.8, 33.1);
.a02.in <- c(66.2, 46.8);
.a04.in <- c(93.6, 66.2);

# a list of goal sizes for images
.ax.in <- list(.a6.in, .a5.in, .a4.in, .a3.in, .a2.in, .a1.in, .a0.in, .a02.in, .a04.in);

#' @title Suggest Proper Width and Height for a Graphic with a Certain Number of
#'   Diagram Rows and Columns
#' @description  Given a number of \code{rows} and \code{columns}, make a
#'   suggestion about the \code{c(width, height)} in inch that a graphic should
#'   have to accommodate the figure.
#' @param rows the rows
#' @param columns the columns
#' @param single.minWidth.in the minimum width of a diagram in inch
#' @param single.minHeight.in the minimum height of a diagram in inch
#' @return a vector \code{c(width, height)} with appropriate width and height in
#'   inch of the graphic
#' @include goldenRatio.R
#' @export plots.size
plots.size <- function(rows=1L, columns=1L,
                       single.minWidth.in = 2.2,
                       single.minHeight.in=max(1, single.minWidth.in/.goldenRatio)) {
# sanity check
  stopifnot((rows >= 1L) && (columns >= 1) &&
            (single.minHeight.in > 0) &&
            (single.minWidth.in  > 0) &&
            is.finite(single.minHeight.in) &&
            is.finite(single.minWidth.in));

# round up the minimum space requirements
  single.minHeight.in <- (ceiling(single.minHeight.in*10))/10;
  single.minWidth.in  <- (ceiling(single.minWidth.in*10))/10;

# try to approximate a reasonable spacing
  spaceX <- max(0.5, 0.05*single.minWidth.in);
  spaceY <- max(0.5, 0.05*single.minHeight.in);
  if((spaceX/.goldenRatio) < spaceY) { spaceX <- .goldenRatio*spaceY; }
  if((spaceY*.goldenRatio) < spaceX) { spaceY <- spaceX/.goldenRatio; }
  spaceX <- ceiling(100*spaceX)/100;
  spaceY <- ceiling(100*spaceY)/100;

# try to approximate reasonable dimensions, including 15% of margins
  requiredX <- (spaceX*(columns - 1L) + (columns * single.minWidth.in))  / 0.85;
  requiredY <- (spaceY*(rows    - 1L) + (rows    * single.minHeight.in)) / 0.85;
  area      <- requiredX * requiredY;

# try to find a good candidate paper size
  for(candidate in .ax.in) {
    a <- candidate[1];
    b <- candidate[2];

    if((a >= requiredX) && (b >= requiredY)) { # landscape
# OK, a page from the A series has been found which is big enough to accommodate
# the figure
      area.c <- a * b;
      if( (area.c * 0.7) <= area ) {
# and its area is not too big
        return(candidate);
      }
    }

    if((b >= requiredX) && (a >= requiredY)) { # portrait
# OK, a page from the A series has been found which is big enough to accommodate
# the figure
      area.c <- a * b;
      if( (area.c * 0.7) <= area ) {
# and its area is not too big
        return(c(b, a));
      }
    }
  }

  # try to see if we can make it fit to the golden ratio
  if(requiredX > requiredY) { # landscape
    a <- max(requiredX, .goldenRatio*requiredY);
    b <- max(requiredY, requiredX/.goldenRatio);
    area.c <- (a*b);
    if( (area.c * 0.7) <= area ) {
      requiredX <- a;
      requiredY <- b;
    }
  } else {
    if(requiredY > requiredX) { # portrait
      a <- max(requiredX, requiredY/.goldenRatio);
      b <- max(requiredY, requiredX*.goldenRatio);
      area.c <- (a*b);
      if( (area.c * 0.7) <= area ) {
        requiredX <- a;
        requiredY <- b;
      }
    } # else: square ... keep square
  }

  # round the sizes ad return
  return( 0.1 * ceiling(10 * c(requiredX, requiredY)) );
}
