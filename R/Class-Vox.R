#' An S4 class to represent voxels
#'
#' @importFrom sf st_crs NA_crs_
#' @importFrom utils head
#'
#' @author Benjamin Brede
#'
#' @description
#' Class Vox is the representation of a 3D grid of regularly spaced volumes, typically produced by voxelization Rays object.
#'
#' @slot data Voxel data with XYZvoxel columns and attributes (depend on mode)
#' @slot extent Voxel grid extent in vox crs. (xmin, ymin, zmin, ymin, ymax, zmax)
#' @slot resolution Voxel resolution in in vox crs. c(x, y, z)
#' @slot mode Voxel mode: LAD (Leaf Area Density metrics) or OCC (occlusion metrics)
#' @slot height_normalized Is Zvoxel height normalized?
#' @slot crs CRS of Vox as \link{st_crs}
#'
#' @exportClass Vox

setClass(
  Class = "Vox",
  package = "voxelizer",
  slots = c(data = "data.frame",
            extent = "numeric",
            resolution = "numeric",
            mode = "character",
            height_normalized = "logical",
            crs = "crs")
)

#' Initialize object vox
#'
#' @author Benjamin Brede
#'
#' @param .Object Object
#' @param ... Additional arguments passed to other methods or functions.
#'
#' @return a vox object
#'
#' @export

setMethod("initialize", "Vox", function(.Object, ...) {

  dims <- c("x", "y", "z")

  .Object@data <- data.frame(Xvoxel = numeric(),
                             Yvoxel = numeric(),
                             Zvoxel = numeric())

  .Object@extent <- c(0, 0, 0, 1, 1, 1)
  names(.Object@extent) <- c(paste0(dims, "min"), paste0(dims, "max"))

  .Object@resolution <- c(1, 1, 1)
  names(.Object@resolution) <- dims

  .Object@mode <- "LAD"
  .Object@height_normalized <- FALSE
  .Object@crs <- NA_crs_

  return(.Object)
})

#' Check if vox is initilized accordingly
#'
#' @author Benjamin Brede
#'
#' @param .Object Object to check
#'
#' @name Check <- TODO

setValidity("Vox", function(object) {

  dims <- paste0(c("X", "Y", "Z"), "voxel")
  missing_columns <- dims[sapply(dims, function(col) !col %in% names(object@data))]

  if (length(missing_columns) != 0) {
    paste('Vox misses columns:', missing_columns, collapse = ',')
  } else if (length(object@resolution) != 3) {
    "Class Vox does only support three dimensional voxels!"
  } else if (length(object@extent) != 6) {
    "Extent should have 6 elements!"
  } else if (!object@mode %in% c("LAD", "OCC")) {
    "Vox can only be of mode LAD or OCC!"
  } else {
    TRUE
  }
})

#' Show object Vox
#'
#' @param object Vox to show
#'
#' @export

setMethod("show", "Vox", function(object) {

  cat("class            : Vox\n")
  cat("voxels           :", nrow(object@data), "\n")
  cat("extent           :", paste(object@extent, collapse = ", "), "(xmin, ymin, zmin, ymin, ymax, zmax)\n")
  cat("resolution       :", object@resolution, "\n")
  cat("mode             :", object@mode, "\n")
  cat("height normalized:", object@height_normalized, "\n")
  cat("coord. ref.      :", sf::st_crs(object)$Name, "\n")

  return(invisible(object))
})

#' Function to display the first few rows of the voxel data contained in a vox object
#'
#' @author Benjamin Brede
#'
#' @param x a Vox object
#' @param ... Additional arguments passed to the head function
#'
#' @return A `data.frame` showing the first few rows of the voxel data
#'
#' @export

head.Vox <- function(x, ...) {
  utils::head(x@data)
}
