test_that("Initial permutation changes", {
  # Create an example vector
  x <- 1:10

  # Compute the initial permutation
  initial_permutation <- permute(x)

  # Check if the initial permutation changes
  expect_false(identical(permute(x), initial_permutation),
               info = "Initial permutation should change randomly, so there is a chance to hit the initial permutation.")
})

test_that("Permuted vector has the same length as the original", {
  x <- 1:10
  permuted_x <- permute(x)

  expect_equal(length(permuted_x), length(x),
               info = "Permuted vector should have the same length as the original vector.")
})

test_that("Permuted vector contains the same elements as the original", {
  x <- 1:10
  permuted_x <- permute(x)

  expect_true(all(sort(permuted_x) == sort(x)),
              info = "Permuted vector should contain the same elements as the original vector.")
})

test_that("Permuting an empty vector returns an empty vector", {
  x <- numeric(0)
  permuted_x <- permute(x)

  expect_equal(permuted_x, x,
               info = "Permuting an empty vector should return an empty vector.")
})

test_that("Permuting a single-element vector returns the same vector", {
  x <- c(42)
  permuted_x <- permute(x)

  expect_equal(permuted_x, x,
               info = "Permuting a single-element vector should return the same vector.")
})

test_that("Multiple calls to permute return different permutations", {
  x <- 1:10

  # Compute two different permutations
  permuted_x1 <- permute(x)
  permuted_x2 <- permute(x)

  expect_false(identical(permuted_x1, permuted_x2),
               info = "Multiple calls to permute should return different permutations (with high probability).")
})
