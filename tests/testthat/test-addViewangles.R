# Read in our example data
#data_file <- system.file("data","H7_LS_F2_H20_200901-120129.laz", package = "voxelizer")
# Create an S4 LAS object
#data <- readLAS(data_file)
# Reduce size due to performance
#data_reduced <- data[1:10]
# Extend the object to a Rays object
#rays_obj <- initialize("Rays", data_reduced)

test_that("Adding viewangles works.", {
  rays_obj <- new("Rays")
  #addViewangles(rays_obj)
  # Check if VZA and VAA attributes have been added
  #expect_true("VZA" %in% names(rays_obj@data))
  #expect_true("VAA" %in% names(rays_obj@data))
})

#TODO Check how to add mock data
