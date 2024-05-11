#' Fast binning of coordinates on a regular grid
#'
#' This function takes a numeric value 'x', which represents a coordinate, and
#' bins it to the nearest grid point based on the specified resolution 'res' and
#' an optional origin point 'origin'.
#'
#' @param x A numeric value representing the coordinate to be binned.
#' @param res The resolution of the grid, which determines the spacing between grid points.
#' @param origin (optional) The origin point of the grid; defaults to 0 if not specified.
#' @return The binned coordinate value, adjusted to the nearest grid point.
#'
#' @export
#'
#' @examples
#' bin(0.1, 1, origin = 0) # returns 0
#' bin(0.9, 1, origin = 0) # returns 1
#' bin(0.1, 1, origin = 0.5) # returns 0.5
#' bin(0.1, 1, origin = 0.1) # returns 0.1
#' bin(0.4, 0.5, origin = 0) # returns 0.5

bin <- function(x, res, origin = 0) {

  floor(0.5 + (x - origin) / res) * res + origin

}
