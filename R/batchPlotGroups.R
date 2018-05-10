#' @title Plot a Set of Curve Groups from a List of Data Groups
#' @description A simple utility method for visualizing a list of data groups.
#' @param data the data object, could be a list of lists or anything
#' @param extract a function which extracts a list of data elements from each
#'   element of \code{data}. This list will then be passed on to
#'   \code{\link{batchPlot.list}}.
#' @param colors the colors to be used for the plot
#' @inheritDotParams batchPlot.list -data -colors
#' @include distinctColors.R
#' @include batchPlotList.R
#' @export batchPlot.groups
batchPlot.groups <- function(data,
                             extract=identity,
                             colors=colors.distinct(length(data))) {

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
  .batchPlot.list(data=data, colors=fullColors, legendColors=colors, ...);
}
