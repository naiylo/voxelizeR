utils::globalVariables(c(
  "x", "y"
))

#' Translate Vox to sds
#'
#' @importFrom dplyr filter full_join mutate select
#' @importFrom stats setNames
#' @importFrom raster crop
#' @importFrom terra sds rast
#'
#' @author Benjamin Brede
#'
#' @description
#' This function translates a Vox object into a SpatRasterDataset (sds) object.
#' It rescales the X and Y coordinates to global coordinates for raster creation,
#' and creates a multi-layer SpatRaster for each feature, where the z dimension is height.
#'
#' @seealso \link{sds}
#'
#' @name vox2sds
#' @aliases vox2sds,Vox-method
#' @rdname vox2sds-methods
#' @docType methods
#' @usage vox2sds(vox, ...)
#'
#' @param vox A Vox object.
#' @param ... Additional arguments passed to other methods or functions.
#'
#' @return A SpatRasterDataset (sds) object.
#'
#' @examples
#' # Load required packages
#' library(lidR)
#' library(pracma)
#' library(dplyr)
#' library(data.table)
#' library(sf)
#' library(terra)
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
#' # Prepare Dem
#' dem_file <- system.file("extdata","UAV4LAI_DEM.tif", package = "voxelizer")
#' dem <- rast(dem_file)
#' raster::crs(dem) <- "EPSG:32631"
#'
#' normalized_vox <- normalize_voxel_height(vox,dem)
#' result <- vox2sds(normalized_vox)
#' show(normalized_vox)
#' show(result)
#'
#' @export

setGeneric("vox2sds",
           function(vox,
                    ...) standardGeneric("vox2sds"))

#' @rdname vox2sds-methods

setMethod("vox2sds",
          signature = "Vox",
          definition = function(vox) {

            vox_df <- vox@data %>%
              # XYZvoxel refers to voxel center
              # only rescale X and Y to global coordinates for raster creation
              mutate(Xvoxel = (Xvoxel + 0.5) * vox@resolution["x"] + vox@extent["xmin"],
                     Yvoxel = (Yvoxel + 0.5) * vox@resolution["y"] + vox@extent["ymin"])

            features <- setdiff(names(vox_df), paste0(c("X", "Y", "Z"), "voxel"))

            # unscaled z coordinates
            # zs <- seq(vox@extent["zmin"] + 0.5 * vox@resolution["z"], vox@extent["zmax"] - 0.5 * vox@resolution["z"], vox@resolution["z"])

            # add corners pixels to each raster layer to guarantee same extent
            # pixels 5 and 6 are the first from the corner to get the resolution right
            corners_pixels <- data.frame(Xvoxel = c(vox@extent["xmin"] + 0.5 * vox@resolution["x"],
                                                    vox@extent["xmin"] + 0.5 * vox@resolution["x"],
                                                    vox@extent["xmax"] - 0.5 * vox@resolution["x"],
                                                    vox@extent["xmax"] - 0.5 * vox@resolution["x"],
                                                    vox@extent["xmin"] + 1.5 * vox@resolution["x"], # another pixel to get the resolution right
                                                    vox@extent["xmin"] + 0.5 * vox@resolution["x"]), # another pixel to get the resolution right
                                         Yvoxel = c(vox@extent["ymin"] + 0.5 * vox@resolution["y"],
                                                    vox@extent["ymax"] - 0.5 * vox@resolution["y"],
                                                    vox@extent["ymin"] + 0.5 * vox@resolution["y"],
                                                    vox@extent["ymax"] - 0.5 * vox@resolution["y"],
                                                    vox@extent["ymin"] + 0.5 * vox@resolution["y"],
                                                    vox@extent["ymin"] + 1.5 * vox@resolution["y"]))

            minZ_scaled <- min(vox@data$Zvoxel)
            maxZ_scaled <- max(vox@data$Zvoxel)
            zs <- minZ_scaled:(maxZ_scaled - 1)
            zs_unscaled <- (zs + 0.5) * vox@resolution["z"] + vox@extent[3]

            # for each feature create a multi-layer SpatRaster (z dimension is height)
            rast_list <- lapply(features, function(feat) {
              zstack <- lapply(minZ_scaled:(maxZ_scaled - 1), function(z) {

                vox_df_layer <- vox_df %>%
                  filter(Zvoxel == z)

                # problem with empty layers: rast() will not produce desired resolution and extent
                # so create explicitly
                if (nrow(vox_df_layer) == 0) {

                  rast(matrix(NA_real_,
                              ncol = (vox@extent["xmax"] - vox@extent["xmin"]) / vox@resolution["x"],
                              nrow = (vox@extent["ymax"] - vox@extent["ymin"]) / vox@resolution["y"]),
                       extent = ext(c(xmin = vox@extent["xmin"],
                                      xmax = vox@extent["xmax"],
                                      ymin = vox@extent["ymin"],
                                      ymax = vox@extent["ymax"])))

                } else {

                  vox_df_layer %>%
                    full_join(corners_pixels, by = c("Xvoxel", "Yvoxel")) %>%
                    rename(x = Xvoxel,
                           y = Yvoxel) %>%
                    select(x, y, all_of(feat)) %>%
                    # digits parameter: prevent trouble with numerical precision
                    rast(type = "xyz",
                         digits = sign(max(vox@resolution[c("x", "y")]))) %>%
                    crop(ext(c(xmin = vox@extent["xmin"],
                               xmax = vox@extent["xmax"],
                               ymin = vox@extent["ymin"],
                               ymax = vox@extent["ymax"])))
                }
              }) %>%
                rast()

              names(zstack) <- zs_unscaled
              # ToDo: CRS assignment
              # crs(zstack) <- vox@crs
              zstack
            })
            rast_sds <- sds(rast_list)
            names(rast_sds) <- features

            if (vox@mode == "LAD") {
              terra::units(rast_sds) <- c("count", "count", rep("m", 5), "m2", rep("m-1", 7))
            } else {
              terra::units(rast_sds) <- rep("count", 4)
            }

            rast_sds
          })
