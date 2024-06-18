test_that("Vox object can be initialized", {
  vox <- new("Vox")
  expect_true(is(vox, "Vox"))
})

test_that("Show method works for Vox", {
  vox <- new("Vox")
  expect_output(print(vox), "class            : Vox")
})

test_that("Head method works for Vox", {
  vox <- new("Vox")
  vox@data <- data.frame(Xvoxel = 1:5, Yvoxel = 1:5, Zvoxel = 1:5)
  expect_equal(nrow(head.Vox(vox)), 5)
})

test_that("Vox object validity", {
  vox <- new("Vox")
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
  vox <- new("Vox")
  expect_equal(vox@mode, "LAD")
})

test_that("Vox extent slot names are correct", {
  vox <- new("Vox")
  expect_equal(names(vox@extent), c("xmin", "ymin", "zmin", "xmax", "ymax", "zmax"))
})

test_that("Vox resolution slot is initialized correctly", {
  vox <- new("Vox")
  expect_equal(names(vox@resolution), c("x", "y", "z"))
})

test_that("Vox mode slot is initialized correctly", {
  vox <- new("Vox")
  expect_equal(vox@mode, "LAD")
})

test_that("Vox height_normalized slot is initialized correctly", {
  vox <- new("Vox")
  expect_false(vox@height_normalized)
})

test_that("Vox crs slot is initialized correctly", {
  vox <- new("Vox")
  expect_equal(vox@crs, sf::NA_crs_)
})

test_that("Vox data slot maintains correct structure", {
  vox <- new("Vox")
  expect_equal(colnames(vox@data), c("Xvoxel", "Yvoxel", "Zvoxel"))
  expect_equal(nrow(vox@data), 0)
})

test_that("Vox data slot can be manipulated", {
  vox <- new("Vox")
  vox@data <- data.frame(Xvoxel = 1:5, Yvoxel = 1:5, Zvoxel = 1:5)
  expect_equal(nrow(vox@data), 5)
  expect_equal(colnames(vox@data), c("Xvoxel", "Yvoxel", "Zvoxel"))
})
