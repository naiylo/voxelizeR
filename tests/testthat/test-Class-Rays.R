# SPDX-FileCopyrightText: 2024 Helmholtz Centre Potsdam - GFZ German Research Centre for Geosciences
# SPDX-FileCopyrightText: 2024 Benjamin Brede
# SPDX-License-Identifier: GPL-3.0-or-later

# Create a Rays object
rays_obj <- new("Rays")

test_that("Initialization of object Rays works.", {
  expect_s4_class(rays_obj, "Rays")
})

test_that("Rays object has additional attributes initialized correctly.", {
  expect_true("Xtraj" %in% names(rays_obj@data))
  expect_true("Ytraj" %in% names(rays_obj@data))
  expect_true("Ztraj" %in% names(rays_obj@data))
  expect_true("Xorigin" %in% names(rays_obj@data))
  expect_true("Yorigin" %in% names(rays_obj@data))
  expect_true("Zorigin" %in% names(rays_obj@data))
  expect_true("XYZisHit" %in% names(rays_obj@data))
  expect_true("IsOccluded" %in% names(rays_obj@data))
})

test_that("Rays object additional attributes are of correct type.", {
  expect_type(rays_obj@data$Xtraj, "double")
  expect_type(rays_obj@data$Ytraj, "double")
  expect_type(rays_obj@data$Ztraj, "double")
  expect_type(rays_obj@data$Xorigin, "double")
  expect_type(rays_obj@data$Yorigin, "double")
  expect_type(rays_obj@data$Zorigin, "double")
  expect_type(rays_obj@data$XYZisHit, "double")
  expect_type(rays_obj@data$IsOccluded, "double")
})

test_that("Rays object has correct header information.", {
  header <- rays_obj@header
  expect_true(header@PHB[["Global Encoding"]][["WKT"]])
  expect_equal(header@PHB[["Version Minor"]], 4L)
  expect_equal(header@PHB[["Point Data Format ID"]], 6L)
})

test_that("Validity check for Rays object works.", {
  expect_true(validObject(rays_obj))
})

test_that("Combining Rays objects works.", {
  rays_obj1 <- new("Rays")
  rays_obj2 <- new("Rays")
  combined_rays <- rbind.Rays(rays_obj1, rays_obj2)
  expect_s4_class(combined_rays, "Rays")
  expect_true(all(names(rays_obj1@data) %in% names(combined_rays@data)))
})

test_that("Handling of empty Rays object works.", {
  rays_obj <- new("Rays")
  rays_obj@data <- rays_obj@data[0, ] # Make the object empty
  expect_true(validObject(rays_obj))
  expect_true(nrow(rays_obj@data) == 0)
  rays_obj <- addViewangles(rays_obj)
  expect_true("VZA" %in% names(rays_obj@data))
  expect_true("VAA" %in% names(rays_obj@data))
  expect_equal(nrow(rays_obj@data), 0)
})

test_that("Show method for Rays object works.", {
  rays_obj <- new("Rays")
  expect_output(show(rays_obj), "class        : Rays")
  expect_output(show(rays_obj), "names        : X, Y, Z, Xtraj, Ytraj, Ztraj, Xorigin, Yorigin, Zorigin, XYZisHit, IsOccluded")
})
