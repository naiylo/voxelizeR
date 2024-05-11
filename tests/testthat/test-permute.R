test_that("Initial permutation changes", {
  # Create an example vector
  x <- 1:10

  # Compute the initial permutation
  initial_permutation <- permute(x)

  # Check if the initial permutation changes
  expect_false(identical(permute(x), initial_permutation),
               info = "Initial permutation should change randomly, so there is a chance to hit the initial permutation.")
})
