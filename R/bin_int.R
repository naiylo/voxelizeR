#' Fast binning of coordinates on a regular grid and returning an integer
#'
#' @author Benjamin Brede
#'
#' @description
#' This function takes a numeric value 'x', which represents a coordinate, and
#' bins it to the nearest grid point based on the specified resolution 'res' and
#' an optional origin point 'origin'. The result is then converted to an integer.
#'
#' @param x A numeric value representing the coordinate to be binned.
#' @param res The resolution of the grid, which determines the spacing between grid points.
#' @param origin (optional) The origin point of the grid; defaults to 0 if not specified.
#'
#' @return An integer representing the binned coordinate value.
#'
#' @export

bin_int <- function(x, res, origin = 0) {

  as.integer(floor(0.5 + (x - origin) / res))

}
