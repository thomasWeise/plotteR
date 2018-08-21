#' @title Create an ECDF Function over ERTs
#' @description Create an Empirical Cumulative Distribution Function (ECDF)
#'   function over Expected Running Times (ERTs), i.e., a list with two membery
#'   \code{x} and \code{y} that stand for the coordinates of the function. This
#'   approach is suitable if we want to plot an ECDF over multiple distinct
#'   problems. We can compute the expected running time of an algorithm on the
#'   given problem to reach the \code{goal}. Over all the ERTs of all problems,
#'   we can then draw the ECDF.
#' @param x the data, maybe a list of [lists of matrices or a list of
#'   vectors/lists], where each element has at least the \code{goal.dim} and
#'   \code{time.dim} dimension.
#' @param goal the goal value, i.e., the value which must be reached to be
#'   counted as success
#' @param goal.dim the dimension where where the goal values can be found
#' @param time.dim the dimension where the time values can be found
#' @param comparator the comparator, usually \code{`<=`} or \code{`>=`}
#' @param time.type the type function the time dimension, should be
#'   \code{as.integer} or \code{as.numeric}
#' @param goal.type the type function the goal dimension, should be
#'   \code{as.integer} or \code{as.numeric}
#' @param time.min the minimum time value to be used for the diagram, or
#'   \code{NA} to use the smallest time value in any run
#' @param time.max the maximum time value to be used, or \code{NA} to pick the
#'   maximum time value of any run
#' @param extract.run a function which can be used to extract the single runs
#'   from the extracted run sets, e.g., \code{identity}
#' @param extract.runs a function which can be used to extract the run sets from
#'   \code{x}, e.g., \code{identity}
#' @return a \code{list(x=c(...), y=c(...))} where the two members \code{x} and
#'   \code{y} will be vectors of corresponding coordinates
#' @export func.ertEcdf
#' @include funUtils.R
#' @include ertFun.R
#' @include ecdfFun.R
func.ertEcdf <- function(x,
                         goal=0,
                         goal.dim=2L,
                         time.dim=1L,
                         comparator=`<=`,
                         time.type=as.integer,
                         goal.type=as.numeric,
                         time.min=1L,
                         time.max=time.type(NA_real_),
                         extract.run=identity,
                         extract.runs=identity) {

  if(comparator(1L, 2L)) { reorder <- rev; }
  else                   { reorder <- identity; }

  # create the search function
  findFun <- .make.findFun(comparator=comparator);

  .not.found <- integer(0);

  # compute the ERTs to reach the goal as tuple (ert, reached)
  return(func.ecdf(x=lapply(X=x,
              FUN=function(runs) {
                 ert <- func.ert(x=extract.runs(runs),
                                 goal.dim=goal.dim,
                                 time.dim=time.dim,
                                 comparator=comparator,
                                 time.type=time.type,
                                 goal.type=as.numeric,

                                 goal.min=goal,
                                 goal.max=goal,
                                 time.max=time.max,
                                 extract.run=extract.run);
                 if(is.null(ert) || (length(ert) != 2L)) { return(.not.found); }
                 i <- findFun(goal, reorder(ert$x));
                 if(i > 0L) {
                   r <- ert$y[length(ert$y) - i + 1L];
                   if(is.finite(r)) {
                     return(c(r, 0L));
                   }
                 }
                 return(.not.found);
               }),
              goal=0,
              goal.dim=2L,
              time.dim=1L,
              comparator=`<=`,
              time.type=as.numeric,
              time.min=time.min,
              time.max=time.max,
              extract.run=identity));
}
