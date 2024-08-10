# Initialize everything we need

# Create an S4 LAS object
data_file <- system.file("extdata","H7_LS_F2_H20_200901-120129.laz", package = "voxelizer")
las <- readLAS(data_file)
epsg(las) <- 32631
laz <- las[1:100]
# Create trajectory for the object
traj_file <- system.file("extdata","H7_LS_F2_H20_200901-120129.traj", package = "voxelizer")
traj <- fread(traj_file, col.names = c('gpstime', 'roll', 'pitch', 'yaw', 'Xorigin', 'Yorigin', 'Zorigin')) %>%
  select(gpstime, Xorigin, Yorigin, Zorigin) %>%
  rename(Xtraj = Xorigin,
         Ytraj = Yorigin,
         Ztraj = Zorigin)
# Create an S4 Rays object
rays <- las2rays(laz, traj)
# Prepare tiles
tiles <- prepare_tiles(c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680),
                       res = c(x = 1, y = 1),
                       tilesize = c(20, 20),
                       crs = 32631)
# Prepare aoi
aoi <- st_bbox(tiles) %>%
  as.numeric() %>%
  setNames(names(st_bbox(tiles)))
# Prepare zrange
zrange <- c(0, 100) %>%
  setNames(c("zmin", "zmax"))
# Prepare res
res = c(x = 1, y = 1, z = 1)

# Prepare Dem
dem_file <- system.file("extdata","UAV4LAI_DEM.tif", package = "voxelizer")
dem <- rast(dem_file)
raster::crs(dem) <- "EPSG:32631"

vox <- suppressWarnings(
  voxelize(
    rays = rays,
    tiles = tiles,
    zrange = zrange,
    res = res,
    ac_single = 0.001,
    voxel_mode = "OCC",
    process_tiles_parallel = 1,
    process_order_tiles = "random"
  )
)

normalized_vox <- normalize_voxel_height(vox,dem)
result <- vox2sds(normalized_vox)

test_that("vox2sds works correctly and transforms a vox to a SpatRasterDataset", {
  expect_true(inherits(result, "SpatRasterDataset"))
})

test_that("Check the extent of the result", {
  extent_result <- terra::ext(result)
  extent_normalized_vox <- normalized_vox@extent

  expect_equal(extent_result$xmin, extent_normalized_vox["xmin"])
  expect_equal(extent_result$xmax, extent_normalized_vox["xmax"])
  expect_equal(extent_result$ymin, extent_normalized_vox["ymin"])
  expect_equal(extent_result$ymax, extent_normalized_vox["ymax"])
})
