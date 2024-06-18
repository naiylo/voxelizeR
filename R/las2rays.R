utils::globalVariables(c(
  "gpstime"
))

#' Translate LAS object to Rays
#'
#' @importFrom methods new
#' @importFrom sf st_crs
#' @importFrom sf st_crs<-
#' @importFrom sf st_bbox
#' @importFrom data.table setkey
#'
#' @author Benjamin Brede
#'
#' @description
#' This function converts a LAS object, which represents 3D point cloud data,
#' into a Rays object. The conversion requires a trajectory dataset that contains
#' the flight path information of the LiDAR sensor.
#'
#' @seealso \code{\link{Rays-class}}
#'
#' @name las2rays
#' @aliases las2rays,LAS,data.table-method
#' @rdname las2rays-methods
#' @docType methods
#' @usage las2rays(las, traj, ...)
#'
#' @param las A LAS object containing the LiDAR point cloud data.
#' @param traj A data table or numeric vector representing the sensor trajectory.
#'             For a data table, it must contain the columns 'Xtraj', 'Ytraj',
#'             'Ztraj', and 'gpstime'. For a numeric vector, it represents the
#'             x, y, and z coordinates of the trajectory.
#' @param ... Additional arguments passed to other methods or functions.
#'
#' @return Returns a Rays object with the trajectory information included.
#'
#' @export

setGeneric("las2rays",
           function(las, traj, ...) standardGeneric("las2rays"))

#' @rdname las2rays-methods

setMethod("las2rays",
          signature = c("LAS", "data.table"),
          definition = function(las, traj) {

            if (!all(sapply(c("Xtraj", "Ytraj", "Ztraj", "gpstime"), function(c) c %in% names(traj))))
              stop("trajectory misses columns!")

            # index Time
            setkey(las@data, gpstime)
            setkey(traj, gpstime)

            # join based on gpstime
            lastraj <- traj[las@data, roll = "nearest"]

            las_with_traj <- las %>%
              add_lasattribute(name = "Xtraj", desc = "Xtraj", x = lastraj$Xtraj) %>%
              add_lasattribute(name = "Ytraj", desc = "Ytraj", x = lastraj$Ytraj) %>%
              add_lasattribute(name = "Ztraj", desc = "Ztraj", x = lastraj$Ztraj) %>%
              add_lasattribute(name = "Xorigin", desc = "Xorigin", x = lastraj$Xtraj) %>%
              add_lasattribute(name = "Yorigin", desc = "Yorigin", x = lastraj$Ytraj) %>%
              add_lasattribute(name = "Zorigin", desc = "Zorigin", x = lastraj$Ztraj) %>%
              # all hits are original hits, not clipped pulses yet
              add_lasattribute(name = "XYZisHit", desc = "XYZisHit", x = 1)

            rays <- methods::new("Rays")

            rays@header <- las_with_traj@header
            rays@header@PHB[["Global Encoding"]][["WKT"]] <- TRUE
            rays@header@PHB[["Version Minor"]] <- 4L
            rays@header@PHB[["Point Data Format ID"]] <- 6L

            rays@crs         <- las_with_traj@crs
            rays@data        <- las_with_traj@data
            rays@index       <- las_with_traj@index

            return(rays)
          })

#' @rdname las2rays-methods

setMethod("las2rays",
          signature = c("LAS", "numeric"),
          definition = function(las, traj) {

            xtraj <- traj[1]
            ytraj <- traj[2]
            ztraj <- traj[3]

            las_with_traj <- las %>%
              add_lasattribute(name = "Xtraj", desc = "Xtraj", x = xtraj) %>%
              add_lasattribute(name = "Ytraj", desc = "Ytraj", x = ytraj) %>%
              add_lasattribute(name = "Ztraj", desc = "Ztraj", x = ztraj) %>%
              add_lasattribute(name = "Xorigin", desc = "Xorigin", x = xtraj) %>%
              add_lasattribute(name = "Yorigin", desc = "Yorigin", x = ytraj) %>%
              add_lasattribute(name = "Zorigin", desc = "Zorigin", x = ztraj) %>%
              # all hits are original hits, not clipped pulses yet
              add_lasattribute(name = "XYZisHit", desc = "XYZisHit", x = 1)

            las_with_traj@header@VLR <- list() # Erase VLR previously written
            las_with_traj@header@PHB[["Global Encoding"]][["WKT"]] <- TRUE
            las_with_traj@header@PHB[["Version Minor"]] <- 4L
            las_with_traj@header@PHB[["Header Size"]] <- 375L
            las_with_traj@header@PHB[["Offset to point data"]] <- 375L
            wkt(las_with_traj@header) <- sf::st_crs(las)$wkt

            rays <- methods::new("Rays")

            rays@crs         <- las_with_traj@crs
            rays@header      <- las_with_traj@header
            rays@data        <- las_with_traj@data
            rays@index       <- las_with_traj@index

            rays
          })
