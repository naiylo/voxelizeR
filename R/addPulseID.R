#' Add Pulse ID to LAS Object
#'
#' @import lidR
#' @import lidR
#'
#' @author Benjamin Brede
#'
#' @description
#' This function adds a pulse ID attribute to an LAS object that has a trajectory.
#'
#' @param las An object of class LAS with added trajectory (\link{addTraj})
#' @param ... Additional arguments passed to other methods or functions.
#'
#' @export

addPulseID = function(las, ...) UseMethod("addPulseID")

#' @export

addPulseID.LAS <- function(las, ...) {

  pulses <- retrieve_pulses(las)
  add_lasattribute(las, x = pulses$pulseID, name = "pulseID", desc = "pulseID")

}
