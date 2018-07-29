#' @title Create a List of Distinct Line Tpes of Length \code{n}
#' @description Create a vector of line types of a given length \code{n} that can
#'   be used in plots. The line types are chosen to be as different as possible,
#'   but sometimes the same line type may occur, since only a given number of
#'   different line types exist in R.
#' @param n the number of different line types to generate
#' @return the vector of line types
#' @export lineTypes.distinct
lineTypes.distinct <- function(n) rep_len(1L:6L, length.out=n)
