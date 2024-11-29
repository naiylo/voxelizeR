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

test_that("Vox object can be initialized", {
  expect_true(is(vox, "Vox"))
})

test_that("Show method works for Vox", {
  expect_output(print(vox), "class            : Vox")
})

test_that("Head method works for Vox", {
  expect_equal(nrow(head.Vox(vox)), 6)
})

test_that("Vox object validity", {
  expect_true(validObject(vox))

  # Invalid mode
  vox@mode <- "INVALID"
  expect_error(validObject(vox), "Vox can only be of mode LAD or OCC!")

  # Invalid extent length
  vox@mode <- "LAD"
  vox@extent <- c(0, 0, 0, 1, 1)
  expect_error(validObject(vox), "Extent should have 6 elements!")

  # Invalid resolution length
  vox@extent <- c(0, 0, 0, 1, 1, 1)
  vox@resolution <- c(1, 1)
  expect_error(validObject(vox), "Class Vox does only support three dimensional voxels!")

  # Missing columns in data
  vox@resolution <- c(1, 1, 1)
  vox@data <- data.frame(Xvoxel = numeric(), Yvoxel = numeric())
  expect_error(validObject(vox), "Vox misses columns: Zvoxel")
})

test_that("Vox mode slot is initialized correctly", {
  expect_equal(vox@mode, "OCC")
})

test_that("Vox extent slot names are correct", {
  expect_equal(names(vox@extent), c("xmin", "ymin", "zmin", "xmax", "ymax", "zmax"))
})

test_that("Vox resolution slot is initialized correctly", {
  expect_equal(names(vox@resolution), c("x", "y", "z"))
})

test_that("Vox height_normalized slot is initialized correctly", {
  expect_false(vox@height_normalized)
})

test_that("Vox data slot maintains correct structure", {
  expect_equal(colnames(vox@data), c("Xvoxel", "Yvoxel", "Zvoxel","Voxel_N", "Hits", "Empty", "Occluded"))
})

test_that("Vox data slot can be manipulated", {
  vox@data <- data.frame(Xvoxel = 1:5, Yvoxel = 1:5, Zvoxel = 1:5)
  expect_equal(nrow(vox@data), 5)
  expect_equal(colnames(vox@data), c("Xvoxel", "Yvoxel", "Zvoxel"))
})
