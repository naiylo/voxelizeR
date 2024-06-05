#TODO are these realistic mock data
vox <- new("Vox",
           data = data.frame(Xvoxel = 1:5, Yvoxel = 1:5, Zvoxel = 1:5),
           resolution = c(x = 1, y = 1, z = 1),
           extent = c(xmin = 0, xmax = 5, ymin = 0, ymax = 5, zmin = 0, zmax = 5),
           mode = "LAD")
transect <- st_linestring(matrix(c(1,1,2,2), ncol=2))
transect_width <- 1

test_that("vox2transect returns a SpatRasterDataset", {
  #result <- vox2transect(vox, transect, transect_width)
  #expect_s3_class(result, "SpatRasterDataset")
})
