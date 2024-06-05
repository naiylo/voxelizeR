#' Translate Vox to transect
#'
#' @importFrom sf st_coordinates st_geometry_type st_as_binary st_as_sf st_intersection st_distance
#'
#' @author Benjamin Brede
#'
#' @description
#' This function translates a Vox object into a transect. It rescales the X and Y coordinates to global coordinates for raster creation,
#' and creates a multi-layer SpatRaster for each feature, where the z dimension is height.
#'
#' @name vox2transect
#' @aliases vox2transect,Vox-method
#' @rdname vox2transect-methods
#' @docType methods
#' @usage vox2transect(vox, transect, transect_width, ...)
#'
#' @param vox A Vox object.
#' @param transect A sf object representing the transect.
#' @param transect_width A numeric value representing the width of the transect.
#' @param ... Additional arguments passed to other methods or functions.
#'
#' @return A SpatRasterDataset (sds) object.
#'
#' @export

setGeneric("vox2transect",
           function(vox,
                    transect,
                    transect_width,...) standardGeneric("vox2transect"))

#' @rdname vox2transect-methods

setMethod("vox2transect", signature = c("Vox", "sf", "numeric"), definition = function(vox, transect, transect_width) {

  if (st_geometry_type(transect[1,]) != "LINESTRING")
    stop("Transect is not a LINESTRING. Please provide LINESTRING!")

  if (nrow(transect) > 1)
    warning("transect contains multiple features. Only the first will be used!")

  transect_coord <- transect[1,] %>%
    st_coordinates()

  # change of X with Y coordinates
  transect_deltaXY <- unname((transect_coord[2, "Y"] - transect_coord[1, "Y"]) / (transect_coord[2, "X"] - transect_coord[1, "X"]))

  a <- 1000

  vox_lad_proj <- .data$vox_lad_raw %>%
    bind_rows() %>%
    mutate(NHeight = .data$Zvoxel - .data$Height + .data$stepXYZ["Z"] / 2,
           Easting = .data$Xvoxel + .data$minXYZ["X"] + .data$stepXYZ["X"] / 2,
           Northing = .data$Yvoxel + .data$minXYZ["Y"] + .data$stepXYZ["X"] / 2)

  vox_lad_proj_map <- vox_lad_proj %>%
    distinct(.data$Easting, .data$Northing)

  # compute projected point on trajectory (D = distance from start point)
  vox_lad_D <- mclapply(1:nrow(vox_lad_proj_map), mc.cores = 15, function(i) {

    vox_lad_proj_map %>%
      slice(i) %>%
      mutate(Easting1 = .data$Easting - a,
             Northing1 = .data$Northing + a / transect_deltaXY,
             Easting2 = .data$Easting + a,
             Northing2 = .data$Northing - a / transect_deltaXY) %>%
      mutate(geometry = list(st_linestring(dim = "XY",
                                           x = matrix(ncol = 2, byrow = TRUE,
                                                      c(.data$Easting1, .data$Northing1,
                                                        .data$Easting2, .data$Northing2))) %>%
                               st_as_binary())) %>%
      st_as_sf(sf_column_name = 'geometry', crs = crs, agr = "constant") %>%
      st_intersection(transect[1,]) %>%
      mutate(D = as.numeric(st_distance(.data$., .data$st_startpoint(transect[1,])))) %>%
      select(-ends_with("1"), -ends_with("2"), -.data$ID)
  }) %>%
    bind_rows()

  vox_lad_sf <- vox_lad_proj %>%
    filter(.data$Voxel_N > 1, .data$NHeight <= 40, .data$NHeight >= 0) %>%
    inner_join(vox_lad_D, by = c("Easting", "Northing")) %>%
    mutate(D_bin = bin(.data$D, res = 1)) %>%
    group_by(.data$D_bin, .data$NHeight) %>%
    summarise(Hits = sum(.data$Hits),
              Voxel_N = sum(.data$Voxel_N),
              MLE = mean(.data$MLE, na.rm = TRUE),
              MCF = mean(.data$MCF, na.rm = TRUE)) %>%
    ungroup()

})
