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

test_that("Test if voxelize works correctly", {
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
  expect_s4_class(vox, "Vox")
  expect_true(all(c("Xvoxel", "Yvoxel", "Zvoxel") %in% colnames(vox@data)))
  expect_true(nrow(vox@data) > 0)
})

test_that("Voxelize function returns NULL for non-intersecting tiles and rays", {
  non_intersecting_tiles <- prepare_tiles(c(xmin = 682500, ymin = 5764000, xmax = 682600, ymax = 5764100),
                                          res = c(x = 1, y = 1),
                                          tilesize = c(20, 20),
                                          crs = 32631)
  vox <- suppressWarnings(
    voxelize(
      rays = rays,
      tiles = non_intersecting_tiles,
      zrange = zrange,
      res = res,
      ac_single = 0.001,
      voxel_mode = "OCC",
      process_tiles_parallel = 1,
      process_order_tiles = "random"
    )
  )
  expect_null(vox)
})

test_that("Voxelize function returns correct CRS", {
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
  expect_equal(st_crs(vox@crs), st_crs(rays))
})

test_that("Voxelize function handles different voxel modes", {
  vox_lad <- suppressWarnings(
    voxelize(
      rays = rays,
      tiles = tiles,
      zrange = zrange,
      res = res,
      ac_single = 0.001,
      voxel_mode = "LAD",
      process_tiles_parallel = 1,
      process_order_tiles = "random"
    )
  )
  expect_s4_class(vox_lad, "Vox")
  expect_true(all(c("Xvoxel", "Yvoxel", "Zvoxel") %in% colnames(vox_lad@data)))
  expect_true(nrow(vox_lad@data) > 0)

  vox_occ <- suppressWarnings(
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
  expect_s4_class(vox_occ, "Vox")
  expect_true(all(c("Xvoxel", "Yvoxel", "Zvoxel") %in% colnames(vox_occ@data)))
  expect_true(nrow(vox_occ@data) > 0)
})

test_that("Voxelize function respects the resolution parameter", {
  custom_res <- c(x = 2, y = 2, z = 2)
  vox <- suppressWarnings(
    voxelize(
      rays = rays,
      tiles = tiles,
      zrange = zrange,
      res = custom_res,
      ac_single = 0.001,
      voxel_mode = "OCC",
      process_tiles_parallel = 1,
      process_order_tiles = "random"
    )
  )
  expect_s4_class(vox, "Vox")
  expect_true(all(c("Xvoxel", "Yvoxel", "Zvoxel") %in% colnames(vox@data)))
  expect_true(nrow(vox@data) > 0)
})

