# SPDX-FileCopyrightText: 2024 Helmholtz Centre Potsdam - GFZ German Research Centre for Geosciences
# SPDX-FileCopyrightText: 2024 Benjamin Brede
# SPDX-License-Identifier: GPL-3.0-or-later

#' Fast binning of coordinates on a regular grid and returning an integer
#'
#' @author Benjamin Brede
#'
#' @description
#' This function takes a numeric value x, which represents a coordinate, and
#' bins it to the nearest grid point based on the specified resolution res and
#' an optional origin point origin. The result is then converted to an integer.
#'
#' @param x A numeric value representing the coordinate to be binned.
#' @param res The resolution of the grid, which determines the spacing between grid points.
#' @param origin (optional) The origin point of the grid; defaults to 0 if not specified.
#'
#' @return An integer representing the binned coordinate value.
#'
#' @examples
#'
#' # Example 1: Basic binning with default origin
#' x1 <- 10.5
#' res1 <- 2
#' bin_result1 <- bin_int(x1, res1)
#' print(bin_result1) # Expected output: 5
#'
#' # Example 2: Binning with a specified origin
#' x2 <- 10.5
#' res2 <- 2
#' origin2 <- 1
#' bin_result2 <- bin_int(x2, res2, origin2)
#' print(bin_result2) # Expected output: 5
#'
#' # Example 3: Binning negative values
#' x3 <- -3.2
#' res3 <- 1
#' bin_result3 <- bin_int(x3, res3)
#' print(bin_result3) # Expected output: -3
#'
#' # Example 4: Binning with a finer resolution
#' x4 <- 5.75
#' res4 <- 0.5
#' bin_result4 <- bin_int(x4, res4)
#' print(bin_result4) # Expected output: 12
#'
#' # Example 5: Binning with a coarse resolution
#' x5 <- 100
#' res5 <- 25
#' bin_result5 <- bin_int(x5, res5)
#' print(bin_result5) # Expected output: 4
#'
#' @export

bin_int <- function(x, res, origin = 0) {

  # Check if res is a positive, non-zero value
  if (res <= 0) {
    stop("Resolution 'res' must be a positive, non-zero value.")
  }

  # Check if the values are numeric
  if (!is.numeric(x)) {
    stop("The coordinate 'x' must be a numeric value.")
  }
  if (!is.numeric(res)) {
    stop("The resolution 'res' must be a numeric value.")
  }

  as.integer(floor(0.5 + (x - origin) / res))

}
