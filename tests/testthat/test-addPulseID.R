# Read in our example data
data_file <- system.file("data","H7_LS_F2_H20_200901-120129.laz", package = "voxelizer")
# Create an S4 LAS object
data <- readLAS(data_file)
# Reduce size due to performance
data_reduced <- data[1:10]

test_that("Adding the PulseID works accordingly.", {
  data_reduced <- addPulseID(data_reduced)
  expect_true("pulseID" %in% names(data_reduced@data))
})
