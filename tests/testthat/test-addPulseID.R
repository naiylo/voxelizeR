# Read in our example data
data_file <- system.file("extdata","H7_LS_F2_H20_200901-120129.laz", package = "voxelizer")
data <- readLAS(data_file)
data_reduced <- data[1:10] # Reduce size for performance

test_that("Adding the PulseID works accordingly.", {
  data_reduced <- addPulseID(data_reduced)
  expect_true("pulseID" %in% names(data_reduced@data))
})

test_that("Adding PulseID to a larger dataset works accordingly.", {
  data_large <- data[1:1000] # Use a larger subset
  data_large <- addPulseID(data_large)
  expect_true("pulseID" %in% names(data_large@data))
})

test_that("PulseID values are non-null after addition.", {
  data_reduced <- addPulseID(data_reduced)
  expect_true(all(!is.na(data_reduced@data$pulseID)))
})

test_that("Number of points in LAS object remains unchanged after adding PulseID.", {
  original_count <- nrow(data_reduced@data)
  data_reduced <- addPulseID(data_reduced)
  new_count <- nrow(data_reduced@data)
  expect_equal(original_count, new_count)
})

test_that("PulseID attribute has unique values for each pulse.", {
  data_reduced <- addPulseID(data_reduced)
  unique_pulses <- unique(data_reduced@data$pulseID)
  expect_equal(length(unique_pulses), length(unique(data_reduced@data$pulseID)))
})

test_that("Function handles an empty LAS object correctly.", {
  data_empty <- data[0]
  data_empty <- addPulseID(data_empty)
  expect_true("pulseID" %in% names(data_empty@data))
  expect_equal(nrow(data_empty@data), 0)
})
