#' Add Pulse ID to LAS Object
#'
#' @importFrom lidR retrieve_pulses
#' @importFrom lidR add_lasattribute
#'
#' @author Benjamin Brede
#'
#' @description
#' This function adds a pulse ID attribute to an LAS object that has a trajectory.
#'
#' @param las An object of class LAS with added trajectory
#' @param ... Additional arguments passed to other methods or functions.
#'
#' @export

addPulseID = function(las, ...) UseMethod("addPulseID")

#' @export

addPulseID.LAS <- function(las, ...) {

  # Check if LAS object is empty
  if (npoints(las) == 0) {
    las@data$pulseID <- integer(0)
    return(las)
  }

  pulses <- lidR::retrieve_pulses(las)
  lidR::add_lasattribute(las, x = pulses$pulseID, name = "pulseID", desc = "pulseID")

}
