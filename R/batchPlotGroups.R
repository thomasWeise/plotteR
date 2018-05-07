#' @title Plot a Set of Curve Groups from a List of Data Groups
#' @description A simple utility method for visualizing a list of data groups.
#' @param data the data object, could be a list of lists or anything
#' @param extract a function which extracts a list of data elements from each
#'   element of \code{data}. This list will then be passed on to
#'   \code{\link{batchPlat.list}}.
#' @param log the names of the axes (\code{x}, \code{y}) that should be
#'   logarithmically scaled
#' @param xfun a function which receives an element from the \code{data} list
#'   and extracts a vector of \code{x}-coordinates from it
#' @param yfun a function which receives an element from the \code{data} list
#'   and extracts a vector of \code{y}-coordinates from it to be plotted as
#'   points, or \code{NULL} if no points should be plotted (see \code{plotXY})
#' @param ffun a function which receives an element from the \code{data} list
#'   and the corresponding extracted \code{x}-coordinates and extracts or
#'   computes a vector of \code{y}-coordinates from it to be plotted as line, or
#'   \code{NULL} if no points should be plotted (see \code{plotXF})
#' @param plotXY should the \code{x-y} points be plotted (if \code{yfun} is not
#'   \code{NULL})
#' @param widthXY the line width for points to be plotted (only considered if
#'   \code{plotXY} is \code{TRUE} and \code{yfun} is not \code{NULL})
#' @param plotXF should the \code{x-y} lines be plotted (if \code{ffun} is not
#'   \code{NULL})
#' @param widthXF the line width for lines to be plotted (only considered if
#'   \code{plotXF} is \code{TRUE} and \code{ffun} is not \code{NULL})
#' @param names the names of the lines to be printed in the legend, or
#'   \code{NULL} if no legend should be plotted
#' @param colors the colors to be used for the plot
#' @param xlab the label for the \code{x}-axis, or the empty string if no label
#'   is needed
#' @param ylab the label for the \code{y}-axis, or the empty string if no label
#'   is needed
#' @param legendPos the position of the legend, if a legend should be printed
#'   (see \code{names}), or \code{NULL} if no legend is needed
#' @include distinctColors.R
#' @include batchPlotList.R
#' @export batchPlot.groups
batchPlot.groups <- function(data,
                             extract=identity,
                             xfun=function(d) d$x,
                             yfun=function(d) d$y,
                             ffun=function(d, x) yfun(d),
                             plotXY=TRUE,
                             widthXY=0.5,
                             plotXF=TRUE,
                             widthXF=1.5,
                             log="",
                             names=NULL,
                             colors=colors.distinct(length(data)),
                             xlab="",
                             ylab="",
                             legendPos="topright") {

  stopifnot(identical(length(data), length(colors)));

  # extract the data items from the groups, i.e., flatten the groups step 1
  data <- lapply(X=data, FUN=extract);
  # replicate the colors so that all elements of a completely flat data list would have the same color
  fullColors <- lapply(X=seq_along(data), FUN=function(i) rep(x=colors[i], times=length(data[[i]])));
  # flatten the data list, step 2
  data <- unlist(x=data, recursive=FALSE, use.names = FALSE);
  # flatten the color list
  fullColors <- unlist(x=fullColors, recursive=TRUE, use.names = FALSE);

  stopifnot(identical(length(data), length(fullColors)));

  # delegate to the list plotting
  .batchPlot.list(data=data, log=log, xfun=xfun, yfun=yfun,
                  ffun=ffun, plotXY=plotXY, widthXY=widthXY,
                  plotXF=plotXF, widthXF=widthXF, names=names,
                  colors=fullColors, legendColors=colors, xlab=xlab,
                  ylab=ylab, legendPos=legendPos);
}
