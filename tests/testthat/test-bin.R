test_that("binning works correctly", {
  # Test cases for default origin = 0
  expect_equal(bin(0.1, 1), 0)
  expect_equal(bin(0.9, 1), 1)
  expect_equal(bin(0.4, 0.5), 0.5)

  # Test cases for specified origin
  expect_equal(bin(0.1, 1, origin = 0.5), 0.5)
  expect_equal(bin(0.1, 1, origin = 0.1), 0.1)

  # Test cases for non-integer resolution
  expect_equal(bin(0.75, 0.25), 0.75)
  expect_equal(bin(0.1, 0.25), 0)
})
