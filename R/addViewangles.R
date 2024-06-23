#' Add View Zenith Angle (VZA) and View Azimuth Angle (VAA) to Rays
#'
#' @importFrom data.table fread
#' @importFrom lidR add_lasattribute
#' @importFrom pracma acosd
#'
#' @author Benjamin Brede
#'
#' @description
#' This function computes the View Zenith Angle (VZA) and View Azimuth Angle (VAA)
#' for an object of class Rays.
#'
#' @name addViewangles
#' @aliases addViewangles,Rays-method
#' @rdname addViewangles-methods
#' @docType methods
#' @usage addViewangles(rays, ...)
#'
#' @param rays An object of class Rays, which contains the trajectory information
#' of the rays including the X, Y, and Z coordinates of the origin and the target
#' trajectory points.
#' @param ... Additional arguments passed to other methods or functions.
#'
#' @return The input Rays object with two new attributes added: VZA and VAA.
#'
#' @examples
#' # Load required packages
#' library(lidR)
#' library(pracma)
#' library(dplyr)
#' library(data.table)
#'
#' # Read in LAS data
#' data_file <- system.file("extdata", "H7_LS_F2_H20_200901-120129.laz", package = "voxelizer")
#' las <- readLAS(data_file)
#'
#' # Create a trajectory for the LAS object
#' traj_file <- system.file("extdata", "H7_LS_F2_H20_200901-120129.traj", package = "voxelizer")
#' colnames <- c('gpstime', 'roll', 'pitch', 'yaw', 'Xorigin', 'Yorigin', 'Zorigin')
#' traj <- fread(traj_file[1], col.names = colnames) %>%
#'   select(gpstime, Xorigin, Yorigin, Zorigin) %>%
#'   rename(Xtraj = Xorigin,
#'          Ytraj = Yorigin,
#'          Ztraj = Zorigin)
#'
#' # Create a subset of the LAS data
#' laz <- las[1:10]
#'
#' # Compute rays from LAS points and trajectory
#' rays_obj <- las2rays(laz, traj)
#'
#' # Add view angles to the rays object
#' rays_obj <- addViewangles(rays_obj)
#'
#' # Check if VZA and VAA attributes have been added
#' names(rays_obj@data)
#'
#' # Verify that VZA values are within expected range (0 to 180 degrees)
#' all(rays_obj@data$VZA >= 0 & rays_obj@data$VZA <= 180)
#'
#' # Verify that VAA values are within expected range (0 to 360 degrees)
#' all(rays_obj@data$VAA >= 0 & rays_obj@data$VAA <= 360)
#'
#' # Example with a larger dataset
#' laz_large <- las[1:1000]
#' rays_large <- las2rays(laz_large, traj)
#' rays_large <- addViewangles(rays_large)
#'
#' @export

setGeneric("addViewangles",
           function(rays, ...) standardGeneric("addViewangles"))

#' @rdname addViewangles-methods

setMethod("addViewangles",
          signature = c("Rays"),
          definition = function(rays) {

            # Check if Rays object is empty
            if (nrow(rays@data) == 0) {
              rays@data$VZA <- numeric(0)
              rays@data$VAA <- numeric(0)
              return(rays)
            }

            vx <- rays$Xtraj - rays$X
            vy <- rays$Ytraj - rays$Y
            vz <- abs(rays$Ztraj - rays$Z)

            vza <- pracma::acosd(vz / sqrt(vx ^ 2 + vy ^ 2 + vz ^ 2))

            vaa <- ifelse(vx <= 0,
                          pracma::acosd(-vy / sqrt(vx ^ 2 + vy ^ 2)),
                          360 - pracma::acosd(-vy / sqrt(vx ^ 2 + vy ^ 2)))

            rays %>%
              lidR::add_lasattribute(x = vza, name = "VZA", desc = "VZA") %>%
              lidR::add_lasattribute(x = vaa, name = "VAA", desc = "VAA")

          })

