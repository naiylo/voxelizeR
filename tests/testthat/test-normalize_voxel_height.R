# SPDX-FileCopyrightText: 2024 Helmholtz Centre Potsdam - GFZ German Research Centre for Geosciences
# SPDX-FileCopyrightText: 2024 Benjamin Brede
# SPDX-License-Identifier: GPL-3.0-or-later

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
zrange <- c(50, 55) %>%
  setNames(c("zmin", "zmax"))
# Prepare res
res = c(x = 1, y = 1, z = 1)

# Prepare Dem
dem_file <- system.file("extdata","UAV4LAI_DEM.tif", package = "voxelizer")
dem <- rast(dem_file)
raster::crs(dem) <- "EPSG:32631"

test_that("normalize_voxel_height function works correctly", {
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
  normalized_vox <- suppressWarnings(normalize_voxel_height(vox, dem))
  expect_true(inherits(normalized_vox, "Vox"))
  # Check if the height has been normalized
  expect_true(min(normalized_vox@data$Zvoxel) != min(vox@data$Zvoxel))

  # Check if the height_normalized flag is set to TRUE
  expect_true(normalized_vox@height_normalized)
})

test_that("normalize_voxel_height handles empty voxels gracefully", {
  empty_vox <- suppressWarnings(voxelize(
    rays = rays,
    tiles = tiles,
    zrange = zrange,
    res = res,
    ac_single = 0.001,
    voxel_mode = "OCC",
    process_tiles_parallel = 1,
    process_order_tiles = "random"
  ))

  # Expect that trying to normalize empty_vox does not throw errors
  normalized_empty_vox <- suppressWarnings(normalize_voxel_height(empty_vox, dem))
  expect_true(inherits(normalized_empty_vox, "Vox"))
  expect_true(normalized_empty_vox@height_normalized)

})

test_that("normalize_voxel_height handles missing or incomplete DEM data", {
  # Prepare a DEM with missing values
  dem_missing <- rast(nrows = 100, ncols = 100)
  values(dem_missing) <- NA_real_
  raster::crs(dem_missing) <- "EPSG:32631"

  vox <- suppressWarnings(voxelize(
    rays = rays,
    tiles = tiles,
    zrange = zrange,
    res = res,
    ac_single = 0.001,
    voxel_mode = "OCC",
    process_tiles_parallel = 1,
    process_order_tiles = "random"
  ))

  normalized_vox_missing_dem <- suppressWarnings(normalize_voxel_height(vox, dem_missing))
  expect_true(inherits(normalized_vox_missing_dem, "Vox"))
  expect_true(normalized_vox_missing_dem@height_normalized)

})

