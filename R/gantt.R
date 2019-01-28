.gantt.min <- -1/3;
.gantt.max <- 1/3;
#' @title Plot a Gantt Chart
#' @description Plot a Gantt chart based on a list \code{x} of lists of data. The
#'   list contains one list for each machine. The first machine at index
#'   \code{1} will be refered to as \code{M0}, the second machine as \code{M1}
#'   and so on. Each machine list, in turn, is a list of lists. Each element has
#'   the form \code{list(job=, start=, end=)}, with elements denoting the job
#'   name, start, and end time, respectively.
#' @param x the x list, a list of lists
#' @param xlab the \code{x} label
#' @param ylab the \code{y} label
#' @param prefix.job the job name prefix
#' @param prefix.machine the machine name prefix
#' @param color.fun a function returning a color for a job
#' @param print.jobs should we print the job ids?
#' @param las the label orientation, see \link[graphics]{par}
#' @param xaxs the x-axis type, see \link[graphics]{par}
#' @param yaxs the y-axis type, see \link[graphics]{par}
#' @inheritDotParams graphics::plot -x -y
#' @export plot.gantt
#' @include distinctColors.R
#' @importFrom graphics plot axis rect text
#' @importFrom grDevices col2rgb
#' @example examples/gantt.R
plot.gantt <- function(x, xlab="Time", ylab="Machine",
                       prefix.job="J",
                       prefix.machine="M",
                       color.fun=colors.distinct,
                       print.jobs=TRUE,
                       las=1L, xaxs="i", yaxs="i", ...) {

  # first, get the range of the xaxis/time axis
  xaxis <- unlist(lapply(X=x,
                     FUN=function(d) {
                       lapply(X=d,
                              FUN=function(dd) c(dd$start, dd$end))
              }));

  # get number of machines
  machines <- length(x);

  # prepare parameters
  pars <- list(...);
  if(is.null(pars$xlab)) {
    pars$xlab <- xlab;
  }
  if(is.null(pars$ylab)) {
    pars$ylab <- ylab;
  }
  if(is.null(pars$xaxs)) {
    pars$xaxs <- xaxs;
  }
  if(is.null(pars$yaxs)) {
    pars$yaxs <- yaxs;
  }
  if(is.null(pars$las)) {
    pars$las <- las;
  }
  pars$type = "n";
  pars$x <- range(xaxis);
  pars$y <- c(.gantt.min, machines- 1 + .gantt.max);
  pars$yaxt <- "n";

  # paint plot area, but without y axis
  do.call(plot, pars);

  # add y axis with machine labels
  M <- (0L:(machines-1));
  axis(2L, at=M,
           labels=vapply(X=M,
                         FUN=function(i) paste(prefix.machine, i, sep="", collapse=""),
                         FUN.VALUE = ""),
       las=las);

  # now get the job names
  jobs <- sort(unique(unlist(lapply(X=x,
                        FUN=function(d) {
                          vapply(X=d,
                                 FUN=function(dd) as.integer(dd$job),
                                 FUN.VALUE = -1L)
                        }))));

  # allocate one color per job
  colors <- color.fun(length(jobs));

  # now paint the chart
  for(i in seq_along(x)) {
    # compute y range for machine
    y.min <- (i - 1 + .gantt.min);
    y.max <- (i - 1 + .gantt.max);
    # iterate over jobs
    for(task in x[[i]]) {
    # only plot non-empty jobs
       if(task$end > task$start) {
        # get job color
        col <- colors[which(jobs == task$job)];
        # paint job
        rect(task$start, y.min, task$end, y.max, col=col, border=NA);

        if(print.jobs) {
          # try to choose a good text color
          col.rgb <- col2rgb(col);
          if(rgb2gray.luminosity(col.rgb[1L], col.rgb[2L], col.rgb[3L]) < 100) {
            text.col = "white";
          } else {
            text.col = "black";
          }

          # add label
          text(x=(0.5*(task$end + task$start)),
               y=(i-1), adj=c(0.5, 0.5),
               cex=1.1,
               labels=paste(prefix.job, task$job, sep="", collapse=""), col=text.col);
        }
      }
    }
  }
}
