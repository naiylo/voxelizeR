# SPDX-FileCopyrightText: 2024 Helmholtz Centre Potsdam - GFZ German Research Centre for Geosciences
# SPDX-FileCopyrightText: 2024 Benjamin Brede
# SPDX-License-Identifier: GPL-3.0-or-later

#' Add Pulse ID to LAS Object
#'
#' @importFrom lidR retrieve_pulses add_lasattribute
#'
#' @author Benjamin Brede
#'
#' @description
#' This function adds a pulse ID attribute to an LAS object that has a trajectory.
#'
#' @param las An object of class LAS with added trajectory
#' @param ... Additional arguments passed to other methods or functions.
#'
#' @examples
#' # Load required packages
#' library(lidR)
#' data_file <- system.file("extdata","H7_LS_F2_H20_200901-120129.laz", package = "voxelizer")
#' data <- readLAS(data_file)
#'
#' # Reduce the LAS object size for faster examples
#' data_reduced <- data[1:10]
#'
#' # Add pulse ID to the reduced dataset
#' data_reduced <- addPulseID(data_reduced)
#'
#' # Check if pulseID attribute is added
#' names(data_reduced@data)
#'
#' @export

addPulseID = function(las, ...) UseMethod("addPulseID")

#' @export

addPulseID.LAS <- function(las, ...) {

  # Check if the las object is valid
  if (!inherits(las, "LAS")) {
    stop("The input 'las' must be an object of class 'LAS'.")
  }

  # Check if LAS object is empty
  if (npoints(las) == 0) {
    las@data$pulseID <- integer(0)
    return(las)
  }

  pulses <- lidR::retrieve_pulses(las)
  lidR::add_lasattribute(las, x = pulses$pulseID, name = "pulseID", desc = "pulseID")

}
