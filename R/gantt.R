.gantt.min <- -1/3;
.gantt.max <- 1/3;
#' @title Plot a Gantt Chart
#' @description Plot a Gantt chart based on a list of lists of \code{data}. The
#'   list contains one list for each machine. The first machine at index
#'   \code{1} will be refered to as \code{M0}, the second machine as \code{M1}
#'   and so on. Each machine list, in turn, is a list of lists. Each element has
#'   the form \code{list(job=, start=, end=)}, with elements denoting the job
#'   name, start, and end time, respectively.
#' @param data the data list, a list of lists
#' @param xlab the \code{x} label
#' @param ylab the \code{y} label
#' @param prefix.job the job name prefix
#' @param prefix.machine the machine name prefix
#' @param color.fun a function returning a color for a job
#' @export plot.gantt
#' @include distinctColors.R
#' @importFrom graphics plot axis rect text
plot.gantt <- function(data, xlab="Time", ylab="Machine",
                       prefix.job="J",
                       prefix.machine="M",
                       color.fun=colors.distinct, ...) {

  # first, get the range of the x/time axis
  x <- unlist(lapply(X=data,
                     FUN=function(d) {
                       lapply(X=d,
                              FUN=function(dd) c(dd$start, dd$end))
              }));

  # get number of machines
  machines <- length(data);

  # paint plot area, but without y axis
  plot(x=range(x), y=c(.gantt.min, machines- 1 + .gantt.max),
       type="n", yaxt="n", xlab=xlab, ylab=ylab,
       ...);

  # add y axis with machine labels
  M <- (0L:(machines-1));
  axis(2, at=M,
          labels=vapply(X=M,
                        FUN=function(i) paste(prefix.machine, i, sep="", collapse=""),
                        FUN.VALUE = ""));

  # now get the job names
  jobs <- sort(unique(unlist(lapply(X=data,
                        FUN=function(d) {
                          vapply(X=d,
                                 FUN=function(dd) as.integer(dd$job),
                                 FUN.VALUE = -1L)
                        }))));

  # allocate one color per job
  colors <- color.fun(length(jobs));

  # now paint the chart
  for(i in seq_along(data)) {
    # compute y range for machine
    y.min <- (i - 1 + .gantt.min);
    y.max <- (i - 1 + .gantt.max);
    # iterate over jobs
    for(task in data[[i]]) {
      # get job color
      col <- colors[which.min(task$job == jobs)];
      # paint job
      rect(task$start, y.min, task$end, y.max, col=col, border=NA);
      # add label
      text(x=(0.5*(task$end + task$start)),
           y=(i-1), adj=c(0.5, 0.5),
           cex=1.1,
           labels=paste(prefix.job, task$job, sep="", collapse=""), col="black");
    }
  }
}
