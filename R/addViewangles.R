#' Add View Zenith Angle (VZA) and View Azimuth Angle (VAA) to Rays
#'
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
#' @export

setGeneric("addViewangles",
           function(rays, ...) standardGeneric("addViewangles"))

#' @rdname addViewangles-methods

setMethod("addViewangles",
          signature = c("Rays"),
          definition = function(rays) {

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

