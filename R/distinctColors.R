# the internal hash
.colors.cache <- new.env();

#' @title Get \code{n} Distinct Colors
#' @description Get a list of \code{n} distinct colors.
#' This is currently just a very preliminary method, not beautiful and far from
#' optimal. For instance, if more colors are required than what the internally
#' pre-defined palettes can do, we try to uniformly sample from the RGB space
#' with the goal to get maximally distant colors.
#' @param n the number of distinct colors we want
#' @return a vector of \code{n} distinct colors
#' @export colors.distinct
#' @include sampleDistinct.R
colors.distinct <- function(n) {
  if(n <= 0L) { return(c()); }

  # see if the table is in the cache
  name <- as.character(n);
  table <- get0(x=name, envir=.colors.cache, inherits=FALSE, ifnotfound=NULL);
  if(!is.null(table)) {
    return(table);
  }

  # find the right color table
  table.use <- NULL;
  hi <- length(colors.distinct.list);
  lo <- 1L;
  # we use binary search
  while(hi >= lo) {
    mid <- (hi + lo) %/% 2L;
    tbl <- colors.distinct.list[[mid]];
    if(tbl$n == n) {
      # ok, we got a perfect table, return it
      return(tbl$colors);
    }
    # no perfect table yet
    if(tbl$n > n) {
      # the table is longer: remember it
      if(is.null(table.use) || (tbl$n < table.use$n)) {
        table.use <- tbl;
      }
      # and look for a shorter table
      hi <- (mid - 1L);
    } else {
      # the table is shorter, don't remember it
      lo <- mid + 1L;
    }
  }

  if(is.null(table.use)) {
    # if we get here, we only have shorter tables, so we need to find
    xyz <- .sampleDistinct(n=n);

    # convert the samples to colors, but only keep unique colors
    res <- unique(vapply(X=seq_len(n),
                   FUN=function(i) {
                    rgb(xyz[1L, i],
                        xyz[2L, i],
                        xyz[3L, i])
                   }, FUN.VALUE = "#000000"));


    # if there are somehow not enough unique colors, add random colors until the
    # length fits
    while(length(res) < n) {
      add <- vapply(X=seq_len(n-length(res)),
                    FUN=function(i) rgb(runif(n=1), runif(n=1), runif(n=1)),
                    FUN.VALUE="#000000");
      res <- unique(c(res, add));
    }

  } else {
    # if we get here, we found a longer table
    # we just return its starting n colors
    res <- table.use$colors[seq_len(n)];
  }

  res <- force(res);
  assign(x=name, envir=.colors.cache, value=res);
  return(res);
}
