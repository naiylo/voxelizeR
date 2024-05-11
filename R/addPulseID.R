#' Add Pulse ID to LAS Object
#'
#' @author Benjamin Brede
#'
#' This function adds a pulse ID attribute to an LAS object that has a trajectory.
#'
#' @param las An object of class LAS with added trajectory (\link{addTraj})
#'
#' @export

addPulseID = function(las, ...) UseMethod("addPulseID")

#' @export

addPulseID.LAS <- function(las) {

  pulses <- retrieve_pulses(las)
  add_lasattribute(las, x = pulses$pulseID, name = "pulseID", desc = "pulseID")

}
