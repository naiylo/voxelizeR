# Create some mock data
vox <- new("Vox",
           data = data.frame(Xvoxel = c(1, 2), Yvoxel = c(3, 4), Zvoxel = c(5, 6)),
           extent = c(xmin = 0, ymin = 0, zmin = 0, xmax = 10, ymax = 10, zmax = 10),
           resolution = c(x = 1, y = 1, z = 1),
           crs = sf::st_crs(4326))

dem <- rast(nrows=10, ncols=10)
values(dem) <- runif(ncell(dem), min = 0, max = 10) # Random heights between 0 and 10
raster::crs(dem) <- "EPSG:4326"


test_that("normalize_voxel_height function works correctly", {
  #TODO the crs of vox is not initialized correctly so it does not work -> ask Benjamin
  print(dem)
  print(vox)
  #normalize_voxel_height(vox, dem)
  #expect_true(inherits(normalized_vox, "Vox"))

  # Check if the height has been normalized
  #expect_true(all(normalized_vox@data$Zvoxel >= 0))

  # Check if the height_normalized flag is set to TRUE
  #expect_true(normalized_vox@height_normalized)
})
