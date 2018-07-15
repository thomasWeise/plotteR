#' @title Create a List of Distinct Symbols of Length \code{n}
#' @description Create a vector of symbols of a given length \code{n} that can
#'   be used in plots. The symbols are chosen to be as different as possible,
#'   but sometimes the same symbols may occur, since only a given number of
#'   different symbols exist in R.
#' @param n the number of different symbols to generate
#' @return the vector of symbols
#' @export symbols.distinct
symbols.distinct <- function(n) rep_len(0L:18L, length.out=n)
