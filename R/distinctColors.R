#' @title Get \code{n} Distinct Colors
#' @description Get a list of \code{n} distinct colors.
#' @param n the number of distinct colors we want
#' @return a vector of \code{n} distinct colors
#' @export colors.distinct
#' @include sampleDistinct.R
colors.distinct <- function(n) {
  if(n <= 0L) { return(c()); }

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

    return(vapply(X=seq_len(n),
                   FUN=function(i) {
                    rgb(xyz[1L, i],
                        xyz[2L, i],
                        xyz[3L, i])
                   }, FUN.VALUE = "#000000"));
  } else {
    # if we get here, we found a longer table
    # we just return its starting n colors
    return(table.use$colors[seq_length(n)]);
  }
}
