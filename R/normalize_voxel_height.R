utils::globalVariables(c(
  "DEMheight"
))


#' Normalize voxel heights by terrain
#'
#' @importFrom raster ncell values<-
#' @importFrom terra rast resample crds
#' @importFrom dplyr rename mutate inner_join
#'
#' @author Benjamin Brede
#'
#' @description
#' This function normalizes the heights of voxels based on a digital elevation model (DEM).
#' It resamples the DEM to match the resolution and extent of the voxels and then adjusts
#' the voxel heights accordingly.
#'
#' @name normalize_voxel_height
#' @aliases normalize_voxel_height,normalize_voxel_height-methods
#' @rdname normalize_voxel_height-methods
#' @docType methods
#'
#' @param vox An object of class Vox representing the voxel data.
#' @param dem An object of class SpatRaster representing the digital elevation model.
#' @param ... Additional arguments passed to other methods or functions.
#'
#' @return An object of class Vox with normalized heights.
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
#' # Normalize voxel heights using the DEM
#' normalized_vox <- normalize_voxel_height(vox, dem)
#'
#' # Check the results
#' print(normalized_vox)
#'
#' @export

normalize_voxel_height <- function(vox, dem, ...) {

  if (!inherits(vox, "Vox")) {
    stop("Input 'vox' must be an object of class 'Vox'")
  }
  if (!inherits(dem, "SpatRaster")) {
    stop("Input 'dem' must be an object of class 'SpatRaster'")
  }

  ## resample dem to Vox resolution and extent
  tmpl <- rast(xmin = vox@extent["xmin"], ymin = vox@extent["ymin"],
               xmax = vox@extent["xmax"], ymax = vox@extent["ymax"],
               resolution = vox@resolution[c("x", "y")],
               crs = vox@crs)

  dem_resample <- resample(dem, tmpl, method = "bilinear")

  # dem as data.frame
  dem_df <- as.data.frame(crds(dem_resample)) %>%
    rename(Xvoxel = x,
           Yvoxel = y) %>%
    mutate(Xvoxel = bin_int(Xvoxel, res = vox@resolution["x"], origin = vox@extent["xmin"]) - 1,
           Yvoxel = bin_int(Yvoxel, res = vox@resolution["y"], origin = vox@extent["ymin"]) - 1,
           DEMheight = bin_int(as.data.frame(dem_resample)[,1], res = vox@resolution["z"], origin = vox@extent["zmin"]))

  vox@data <- vox@data %>%
    inner_join(dem_df, by = c('Xvoxel', 'Yvoxel'), relationship = "many-to-many") %>%
    mutate(Zvoxel = Zvoxel - DEMheight)

  vox@extent["zmin"] <- 0
  vox@extent["zmax"] <- max(vox@data$Zvoxel, na.rm = TRUE) * vox@resolution["z"]

  vox@height_normalized <- TRUE

  vox
}
