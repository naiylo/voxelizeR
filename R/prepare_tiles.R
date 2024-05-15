#' Prepare tiles that define the voxelization region
#'
#' @import dplyr
#' @import sf
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
#' @param ... Additional arguments passed to other methods or functions e.g. crs: CRS for tiles as in \code{\link[sf]{st_as_sf}}
#'
#' @return sf-object
#'
#' @note The function assumes that the input AOI is in the same CRS as specified by the
#' 'crs' parameter.If the CRS does not match, the function may produce incorrect results.
#'
#' @export

prepare_tiles <- function(aoi, res, tilesize, ...) UseMethod("prepare_tiles")

#' @export

prepare_tiles.sfc <- function(aoi,
                              res = c(x = 1, y = 1),
                              tilesize = c(10, 10), ...) {

  bb <- st_bbox(aoi)
  xymin <- floor(bb[c("xmin", "ymin")] / res) * res
  tilen <- ceiling((bb[c("xmax", "ymax")] - xymin) / tilesize)
  xymax <- xymin + tilen * tilesize

  # extended bb
  bb_ext <- st_polygon(list(matrix(ncol = 2, byrow = TRUE,
                                   c(xymin[1], xymin[2],
                                     xymax[1], xymin[2],
                                     xymax[1], xymax[2],
                                     xymin[1], xymax[2],
                                     xymin[1], xymin[2])))) %>%
    st_sfc(crs = st_crs(aoi))


  # g[aoi] selects cells that intersect with aoi
  # only select intersecting
  st_make_grid(bb_ext, n = tilen)[aoi] %>%
    st_sf()
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

  aoi %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_geometry() %>%
    st_sfc(crs = crs) %>%
    prepare_tiles(res = res, tilesize = tilesize)
}
