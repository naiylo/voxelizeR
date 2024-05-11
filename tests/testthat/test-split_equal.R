test_that("split_equal splits vector into equal parts", {
  # Create an example vector
  x <- 1:10
  n <- 5

  # Split the vector
  split_result <- split_equal(x, n)

  # Check if the length of the result is equal to n
  expect_equal(length(split_result), n,
               info = "Length of result should be equal to n")

})

test_that("split_equal2 splits vector into parts of specified length", {
  # Create an example vector
  x <- 1:10
  l <- 5

  # Split the vector
  split_result <- split_equal2(x, l)

  # Check if each part has the specified length
  expect_equal(10/length(split_result), l,
               info = "Each part should have the specified length")
})
