# Create an S4 LAS object
data_file <- system.file("extdata","H7_LS_F2_H20_200901-120129.laz", package = "voxelizer")
las <- readLAS(data_file)
laz <- las[1]
# Create trajectory for the object
traj_file <- system.file("extdata","H7_LS_F2_H20_200901-120129.traj", package = "voxelizer")
traj <- fread(traj_file[1], col.names = c('gpstime', 'roll', 'pitch', 'yaw', 'Xorigin', 'Yorigin', 'Zorigin')) %>%
  select(gpstime, Xorigin, Yorigin, Zorigin) %>%
  rename(Xtraj = Xorigin,
         Ytraj = Yorigin,
         Ztraj = Zorigin)
rays <- las2rays(laz, traj)

test_that("las2rays function returns an object of class 'Rays'", {
  expect_s4_class(rays, "Rays")
})

test_that("las2rays adds trajectory attributes correctly", {
  rays <- las2rays(las, traj)

  # Assertions
  expect_s4_class(rays, "Rays")
  expect_true(all(c("Xtraj", "Ytraj", "Ztraj") %in% names(rays@data)))
  expect_equal(nrow(rays@data), nrow(las))
})

test_that("las2rays handles missing columns in trajectory", {
  # Create a trajectory with missing columns
  traj_missing_cols <- traj[, -c("Xtraj", "Ytraj", "Ztraj")]

  # Expecting an error when columns are missing
  expect_error(las2rays(las, traj_missing_cols), "trajectory misses columns!")
})

test_that("las2rays assigns correct CRS to Rays object", {
  rays <- las2rays(las, traj)

  # Assertions
  expect_s4_class(rays, "Rays")
  expect_identical(st_crs(rays), st_crs(las))
})

test_that("las2rays modifies header correctly", {
  rays <- las2rays(las, traj)

  # Assertions
  expect_s4_class(rays, "Rays")
  expect_equal(rays@header@PHB[["Global Encoding"]][["WKT"]], TRUE)
  expect_equal(rays@header@PHB[["Version Minor"]], 4L)
  expect_equal(rays@header@PHB[["Point Data Format ID"]], 6L)
})



