# SPDX-FileCopyrightText: 2024 Helmholtz Centre Potsdam - GFZ German Research Centre for Geosciences
# SPDX-FileCopyrightText: 2024 Benjamin Brede
# SPDX-License-Identifier: GPL-3.0-or-later

# Create an S4 LAS object
data_file <- system.file("extdata","H7_LS_F2_H20_200901-120129.laz", package = "voxelizer")
las <- readLAS(data_file)
laz <- las[1:100]
# Create trajectory for the object
traj_file <- system.file("extdata","H7_LS_F2_H20_200901-120129.traj", package = "voxelizer")
traj <- fread(traj_file, col.names = c('gpstime', 'roll', 'pitch', 'yaw', 'Xorigin', 'Yorigin', 'Zorigin')) %>%
  select(gpstime, Xorigin, Yorigin, Zorigin) %>%
  rename(Xtraj = Xorigin,
         Ytraj = Yorigin,
         Ztraj = Zorigin)
# Create an S4 Rays object
rays <- las2rays(laz,traj)

test_that("splitMultiReturn does not change the number of rows", {
  original_rays <- rays
  modified_rays <- splitMultiReturn(rays)
  expect_equal(nrow(original_rays@data), nrow(modified_rays@data))
})

test_that("splitMultiReturn does not change the origin coordinates for single-return points", {
  original_rays <- rays
  modified_rays <- splitMultiReturn(rays)
  single_return_indices <- which(original_rays@data$NumberOfReturns == 1)
  expect_equal(modified_rays@data$Xorigin[single_return_indices], original_rays@data$Xorigin[single_return_indices])
  expect_equal(modified_rays@data$Yorigin[single_return_indices], original_rays@data$Yorigin[single_return_indices])
  expect_equal(modified_rays@data$Zorigin[single_return_indices], original_rays@data$Zorigin[single_return_indices])
})

test_that("splitMultiReturn output data structure is consistent", {
  modified_rays <- splitMultiReturn(rays)
  expect_equal(class(modified_rays), class(rays))
})
