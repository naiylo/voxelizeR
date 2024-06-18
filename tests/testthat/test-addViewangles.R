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

test_that("Adding viewangles works.", {
  rays_obj <- las2rays(laz,traj)
  rays_obj <- addViewangles(rays_obj)
  # Check if VZA and VAA attributes have been added
  expect_true("VZA" %in% names(rays_obj@data))
  expect_true("VAA" %in% names(rays_obj@data))
})

test_that("VZA and VAA values are within expected ranges.", {
  rays_obj <- las2rays(laz, traj)
  rays_obj <- addViewangles(rays_obj)
  # VZA should be between 0 and 180 degrees
  expect_true(all(rays_obj@data$VZA >= 0 & rays_obj@data$VZA <= 180))
  # VAA should be between 0 and 360 degrees
  expect_true(all(rays_obj@data$VAA >= 0 & rays_obj@data$VAA <= 360))
})

test_that("Adding viewangles to a larger dataset works accordingly.", {
  laz_large <- las[1:1000] # Use a larger subset
  rays_obj <- las2rays(laz_large, traj)
  rays_obj <- addViewangles(rays_obj)
  # Check if VZA and VAA attributes have been added
  expect_true("VZA" %in% names(rays_obj@data))
  expect_true("VAA" %in% names(rays_obj@data))
})

test_that("Function handles an empty Rays object correctly.", {
  rays_empty <- new("Rays")
  # Check if the normal object gets returned
  expect_false("VZA" %in% names(rays_empty@data))
  expect_false("VAA" %in% names(rays_empty@data))
  expect_equal(nrow(rays_empty@data), 0)
})

test_that("Function handles different trajectories correctly.", {
  # Create a modified trajectory
  traj_mod <- traj
  traj_mod$Xtraj <- traj_mod$Xtraj + 10
  traj_mod$Ytraj <- traj_mod$Ytraj + 10
  traj_mod$Ztraj <- traj_mod$Ztraj + 10

  rays_obj <- las2rays(laz, traj_mod)
  rays_obj <- addViewangles(rays_obj)
  # Check if VZA and VAA attributes have been added
  expect_true("VZA" %in% names(rays_obj@data))
  expect_true("VAA" %in% names(rays_obj@data))
})
