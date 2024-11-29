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

test_that("Clip_rays handles trivial acceptance correctly", {
  clipped_rays <- suppressWarnings(clip_rays(rays, aoi, zrange,buffer <- c(0, 0, 0)))
  expect_s4_class(clipped_rays, "Rays")
})

test_that("Clip_rays handles trivial rejection correctly", {
  aoi <- c(xmin = 682500, ymin = 5764000, xmax = 682600, ymax = 5764100)
  zrange <- c(zmin = 60, zmax = 65)
  buffer <- c(0, 0, 0)

  clipped_rays <- clip_rays(rays, aoi, zrange, buffer)
  expect_s4_class(clipped_rays, "Rays")
  expect_equal(nrow(clipped_rays@data), 0)
})

test_that("Clip_rays handles buffer correctly", {
  aoi <- c(xmin = 682150, ymin = 5763550, xmax = 682350, ymax = 5763650)
  zrange <- c(zmin = 51, zmax = 54)
  buffer <- c(10, 10, 1)

  clipped_rays <- suppressWarnings(clip_rays(rays, aoi, zrange, buffer))

  expect_s4_class(clipped_rays, "Rays")
  expect_true(nrow(clipped_rays@data) > 0)
  expect_true(all(clipped_rays@data$X >= aoi["xmin"] - buffer[1] &
                    clipped_rays@data$X <= aoi["xmax"] + buffer[1]))
  expect_true(all(clipped_rays@data$Y >= aoi["ymin"] - buffer[2] &
                    clipped_rays@data$Y <= aoi["ymax"] + buffer[2]))
  expect_true(all(clipped_rays@data$Z >= zrange["zmin"] - buffer[3] &
                    clipped_rays@data$Z <= zrange["zmax"] + buffer[3]))
})

test_that("Clip_rays handles occlusion extension correctly", {
  aoi <- c(xmin = 682150, ymin = 5763550, xmax = 682350, ymax = 5763650)
  zrange <- c(zmin = 51, zmax = 54)
  buffer <- c(0, 0, 0)
  extension <- 1000

  clipped_rays <- suppressWarnings(clip_rays(rays, aoi, zrange, buffer, occ_extend = TRUE, extension))

  expect_s4_class(clipped_rays, "Rays")
  expect_true(nrow(clipped_rays@data) > 0)
  expect_true(any(clipped_rays@data$IsOccluded == 1))
})

test_that("Clip_rays handles empty input correctly", {
  empty_rays <- new("Rays")
  aoi <- c(xmin = 682150, ymin = 5763550, xmax = 682350, ymax = 5763650)
  zrange <- c(zmin = 51, zmax = 54)
  buffer <- c(0, 0, 0)

  clipped_rays <- suppressWarnings(clip_rays(empty_rays, aoi, zrange, buffer))

  expect_s4_class(clipped_rays, "Rays")
  expect_equal(nrow(clipped_rays@data), 0)
})
