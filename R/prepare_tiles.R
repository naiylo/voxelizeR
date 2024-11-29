# SPDX-FileCopyrightText: 2024 Helmholtz Centre Potsdam - GFZ German Research Centre for Geosciences
# SPDX-FileCopyrightText: 2024 Benjamin Brede
# SPDX-License-Identifier: GPL-3.0-or-later

#' Prepare tiles that define the voxelization region
#'
#' @importFrom sf st_bbox st_sfc st_make_grid st_sf st_as_sfc st_geometry st_crs st_polygon st_set_crs st_linestring
#'
#' @author Benjamin Brede
#'
#' @description
#' This function prepares a grid of tiles based on an area of interest (AOI) for voxelization.
#' It calculates a bounding box for the AOI and creates a grid of tiles with specified resolution
#' and tile size, ensuring that the entire AOI is covered.
#'
#' @param aoi AOI that defines the voxelization region
#' @param res numeric. Resolution for AOI as c(xres, yres)
#' @param tilesize size of tiles
#' @param ... Additional arguments passed to other methods or functions e.g. crs: CRS for tiles as in st_as_sf
#'
#' @return sf-object
#'
#' @note The function assumes that the input AOI is in the same CRS as specified by the
#' crs parameter.If the CRS does not match, the function may produce incorrect results.
#'
#' @examples
#' library(sf)
#'
#' # Example 1: Prepare tiles for a numeric AOI
#' numeric_aoi <- c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680)
#' tiles <- prepare_tiles(numeric_aoi, res = c(1, 1), tilesize = c(20, 20), crs = 32631)
#' print(tiles)
#'
#' # Example 2: Prepare tiles for a bounding box AOI
#' bbox_aoi <- st_bbox(c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680),
#'                       crs = st_crs(32631))
#' tiles <- prepare_tiles(bbox_aoi, res = c(1, 1), tilesize = c(20, 20), crs = 32631)
#' print(tiles)
#'
#' # Example 3: Prepare tiles for an sfc AOI
#' sfc_aoi <- st_as_sfc(st_bbox(c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680),
#'                                crs = 32631))
#' tiles <- prepare_tiles(sfc_aoi, res = c(1, 1), tilesize = c(20, 20))
#' print(tiles)
#'
#' # Example 4: Prepare tiles for an sf AOI
#' sf_aoi <- st_as_sf(sfc_aoi)
#' tiles <- prepare_tiles(sf_aoi, res = c(1, 1), tilesize = c(20, 20))
#' print(tiles)
#'
#' # Example 5: Prepare tiles with different resolutions
#' tiles <- prepare_tiles(numeric_aoi, res = c(2, 2), tilesize = c(20, 20), crs = 32631)
#' print(tiles)
#'
#' # Example 6: Prepare tiles with different tile sizes
#' tiles <- prepare_tiles(numeric_aoi, res = c(1, 1), tilesize = c(40, 40), crs = 32631)
#' print(tiles)
#'
#' @export

prepare_tiles <- function(aoi, res, tilesize, ...) UseMethod("prepare_tiles")

#' @export

prepare_tiles.sfc <- function(aoi,
                              res = c(x = 1, y = 1),
                              tilesize = c(10, 10), ...) {

  # Validate CRS
  if (is.null(st_crs(aoi))) {
    stop("Error: The AOI must have a defined CRS.")
  }

  bb <- st_bbox(aoi)
  xymin <- floor(bb[c("xmin", "ymin")] / res) * res
  tilen <- ceiling((bb[c("xmax", "ymax")] - xymin) / tilesize)
  xymax <- xymin + tilen * tilesize

  # Extended bounding box
  bb_ext <- st_polygon(list(matrix(ncol = 2, byrow = TRUE,
                                   c(xymin[1], xymin[2],
                                     xymax[1], xymin[2],
                                     xymax[1], xymax[2],
                                     xymin[1], xymax[2],
                                     xymin[1], xymin[2])))) %>%
    st_sfc(crs = st_crs(aoi))

  # Create grid of tiles
  grid <- st_make_grid(bb_ext, n = tilen)

  if (is.null(grid)) {
    stop("Error: Failed to create a grid of tiles. Please check the inputs.")
  }

  # Select tiles that intersect with AOI
  st_sf(geometry = grid[aoi])
}

#' @export

prepare_tiles.sf <- function(aoi,
                             res = c(x = 1, y = 1),
                             tilesize = c(10, 10), ...) {
  prepare_tiles(st_as_sfc(aoi),
                res = res,
                tilesize = tilesize)
}

#' @export
prepare_tiles.bbox <- function(aoi,
                               res = c(x = 1, y = 1),
                               tilesize = c(10, 10),
                               crs, ...) {

  if (missing(crs)) {
    stop("Error: CRS must be provided for bbox input.")
  }

  # Convert bbox to sfc with CRS
  st_as_sfc(aoi) %>%
    st_geometry() %>%
    st_sfc(crs = crs) %>%
    prepare_tiles(res = res, tilesize = tilesize)
}

#' @export

prepare_tiles.bbox <- function(aoi,
                               res = c(x = 1, y = 1),
                               tilesize = c(10, 10),
                               crs, ...) {

  if (missing(crs)) {
    stop("Error: CRS must be provided for bbox input.")
  }

  # Convert bbox to sfc with CRS
  st_as_sfc(aoi) %>%
    st_geometry() %>%
    st_sfc(crs = crs) %>%
    prepare_tiles(res = res, tilesize = tilesize)
}

#' @export

prepare_tiles.numeric <- function(aoi,
                                  res = c(x = 1, y = 1),
                                  tilesize = c(10, 10),
                                  crs, ...) {

  if (length(aoi) != 4 || !all(c("xmin", "ymin", "xmax", "ymax") %in% names(aoi))) {
    stop("Error: AOI must be a numeric vector of length 4 with named elements 'xmin', 'ymin', 'xmax', 'ymax'.")
  }
  if (missing(crs)) {
    stop("Error: CRS must be provided for numeric AOI input.")
  }

  # Convert numeric AOI to bbox and then to sfc
  aoi %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_geometry() %>%
    st_sfc(crs = crs) %>%
    prepare_tiles(res = res, tilesize = tilesize)
}
