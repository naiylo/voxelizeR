#' Add View Zenith Angle (VZA) and View Azimuth Angle (VAA) to Rays
#'
#' @author Benjamin Brede
#'
#' This function computes the View Zenith Angle (VZA) and View Azimuth Angle (VAA)
#' for an object of class Rays.
#'
#' @param rays An object of class Rays, which contains the trajectory information
#' of the rays including the X, Y, and Z coordinates of the origin and the target
#' trajectory points.
#'
#' @return The input Rays object with two new attributes added: VZA and VAA.
#'
#' @export

setGeneric("addViewangles",
           function(rays, ...) standardGeneric("addViewangles"))

setMethod("addViewangles",
          signature = c("Rays"),
          definition = function(rays) {

            vx <- rays$Xtraj - rays$X
            vy <- rays$Ytraj - rays$Y
            vz <- abs(rays$Ztraj - rays$Z)

            vza <- acosd(vz / sqrt(vx ^ 2 + vy ^ 2 + vz ^ 2))

            vaa <- ifelse(vx <= 0,
                          acosd(-vy / sqrt(vx ^ 2 + vy ^ 2)),
                          360 - acosd(-vy / sqrt(vx ^ 2 + vy ^ 2)))

            rays %>%
              add_lasattribute(x = vza, name = "VZA", desc = "VZA") %>%
              add_lasattribute(x = vaa, name = "VAA", desc = "VAA")

          })

