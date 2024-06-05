# Create a mock Vox object
#TODO is this voxel realistic tot st or is anything missing to convert it to a sds
vox <- new("Vox",
           data = data.frame(Xvoxel = 1:5, Yvoxel = 1:5, Zvoxel = 1:5),
           resolution = c(x = 1, y = 1, z = 1),
           extent = c(xmin = 0, xmax = 5, ymin = 0, ymax = 5, zmin = 0, zmax = 5),
           mode = "LAD")

#result <- vox2sds(vox)

test_that("vox2sds works correctly and transforms a vox to a SpatRasterDataset", {
  #expect_s3_class(result, "SpatRasterDataset")
})

test_that("Check the dimensions of the result", {
  #expect_equal(dim(result), c(5, 5, 5))
})

test_that("Check the resolution of the result", {
  #expect_equal(dim(result), c(1, 1))
})

test_that("Check the extent of the result", {
  #expect_equal(extent(result), c(0, 5, 0, 5))
})

test_that("vox2sds keeps the resolution", {
  #expect_equal(terra::units(result), c("count", "count", rep("m", 5), "m2", rep("m-1", 7)))
})
