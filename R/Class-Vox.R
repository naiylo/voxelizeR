# SPDX-FileCopyrightText: 2024 Helmholtz Centre Potsdam - GFZ German Research Centre for Geosciences
# SPDX-FileCopyrightText: 2024 Benjamin Brede
# SPDX-License-Identifier: GPL-3.0-or-later

#' An S4 class to represent voxels
#'
#' @importFrom sf st_crs NA_crs_
#' @importFrom utils head
#'
#' @author Benjamin Brede
#'
#' @description
#' Class Vox is the representation of a 3D grid of regularly spaced volumes, typically produced by voxelization of Rays object.
#'
#' @slot data Voxel data with XYZvoxel columns and attributes (depend on mode)
#' @slot extent Voxel grid extent in vox crs. (xmin, ymin, zmin, ymin, ymax, zmax)
#' @slot resolution Voxel resolution in in vox crs. c(x, y, z)
#' @slot mode Voxel mode: LAD (Leaf Area Density metrics) or OCC (occlusion metrics)
#' @slot height_normalized Is Zvoxel height normalized?
#' @slot crs CRS of Vox as \link{st_crs}
#'
#' @examples
#' # Load required packages
#' library(lidR)
#' library(pracma)
#' library(dplyr)
#' library(data.table)
#' library(sf)
#'
#' # Read in LAS data
#' data_file <- system.file("extdata", "H7_LS_F2_H20_200901-120129.laz", package = "voxelizer")
#' las <- readLAS(data_file)
#'
#' # Ensure the CRS of the LAS object
#' epsg(las) <- 32631
#'
#' # Create a trajectory for the LAS object
#' traj_file <- system.file("extdata", "H7_LS_F2_H20_200901-120129.traj", package = "voxelizer")
#' colnames <- c('gpstime', 'roll', 'pitch', 'yaw', 'Xorigin', 'Yorigin', 'Zorigin')
#' traj <- fread(traj_file, col.names = colnames) %>%
#'   select(gpstime, Xorigin, Yorigin, Zorigin) %>%
#'   rename(Xtraj = Xorigin,
#'          Ytraj = Yorigin,
#'          Ztraj = Zorigin) %>%
#'   mutate(gpstime = as.numeric(gpstime),
#'          Xtraj = as.numeric(Xtraj),
#'          Ytraj = as.numeric(Ytraj),
#'          Ztraj = as.numeric(Ztraj))
#'
#' # Create a subset of the LAS data
#' laz <- las[1:10,]
#'
#' # Compute rays from LAS points and trajectory
#' rays <- las2rays(laz, traj)
#'
#' # Prepare tiles
#' tiles <- prepare_tiles(c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680),
#'                        res = c(x = 1, y = 1),
#'                        tilesize = c(20, 20),
#'                        crs = 32631)
#'
#' # Prepare aoi
#' aoi <- st_bbox(tiles) %>%
#'   as.numeric() %>%
#'   setNames(names(st_bbox(tiles)))
#'
#' # Prepare zrange
#' zrange <- c(50, 55) %>%
#'   setNames(c("zmin", "zmax"))
#'
#' # Prepare res
#' res <- c(x = 1, y = 1, z = 1)
#'
#' # Use voxelize function
#' vox <- suppressWarnings(voxelize(
#'   rays = rays,
#'   tiles = tiles,
#'   zrange = zrange,
#'   res = res,
#'   ac_single = 0.001,
#'   voxel_mode = "OCC",
#'   process_tiles_parallel = 1,
#'   process_order_tiles = "random"
#' ))
#'
#' # Check slot values
#' print(vox@mode)
#' print(vox@height_normalized)
#' print(vox@crs)
#' print(vox@extent)
#' print(vox@resolution)
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
#' @param crs CRS of Vox as \link{st_crs}
#' @param ... Additional arguments passed to other methods or functions.
#'
#' @return a vox object
#'
#' @export

setMethod("initialize", "Vox", function(.Object, crs = NA_crs_, ...) {

  dims <- c("x", "y", "z")

  # Check that CRS is valid
  if (!inherits(crs, "crs") && !inherits(crs, "CRS") && !is(crs, "numeric") && !is.na(crs)) {
    stop("Invalid CRS provided. CRS must be of type sf::crs, numeric, or NA.")
  }

  .Object@data <- data.frame(Xvoxel = numeric(),
                             Yvoxel = numeric(),
                             Zvoxel = numeric())

  # Extent initialization
  .Object@extent <- c(0, 0, 0, 1, 1, 1)
  names(.Object@extent) <- c(paste0(dims, "min"), paste0(dims, "max"))

  # Ensure resolution is a valid length 3 vector
  .Object@resolution <- c(1, 1, 1)
  if (length(.Object@resolution) != 3) {
    stop("Resolution must have exactly 3 elements (x, y, z).")
  }
  names(.Object@resolution) <- dims

  .Object@mode <- "LAD"
  .Object@height_normalized <- FALSE
  .Object@crs <- st_crs(crs)

  return(.Object)
})

#' Check if Object is initialized accordingly
#'
#' @name Check (Vox)
#'
#' @author Benjamin Brede
#'
#' @param .Object Object to check

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
  cat("extent           :", paste(object@extent, collapse = ", "), "(xmin, ymin, zmin, xmax, ymax, zmax)\n")
  cat("resolution       :", object@resolution, "\n")
  cat("mode             :", object@mode, "\n")
  cat("height normalized:", object@height_normalized, "\n")
  cat("coord. ref.      :", sf::st_crs(object@crs)$input, "\n")

  return(invisible(object))
})

#' Display the First Few Rows of a Vox Object
#'
#' @param x A Vox object.
#' @param ... Additional arguments passed to the `head` function.
#'
#' @return A `data.frame` showing the first few rows of the voxel data.
#'
#' @method head Vox
#' @rdname head.Vox
#'
#' @export

head.Vox <- function(x, ...) {
  utils::head(x@data)
}
