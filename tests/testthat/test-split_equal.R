test_that("split_equal returns the original vector when n is 1", {
  x <- 1:10
  n <- 1
  split_result <- split_equal(x, n)
  expect_equal(split_result[[1]], x, info = "When n is 1, the function should return the original vector in a list")
})

test_that("split_equal handles empty vector", {
  x <- integer(0)
  n <- 3
  split_result <- split_equal(x, n)
  expect_equal(length(split_result), n, info = "Length of result should be equal to n")
  expect_true(all(sapply(split_result, length) == 0), info = "All groups should be empty for an empty vector")
})

# Additional tests for split_equal2
test_that("split_equal2 handles vectors where length is not a multiple of l", {
  x <- 1:10
  l <- 3
  split_result <- split_equal2(x, l)
  expect_equal(length(split_result), ceiling(length(x) / l), info = "Number of groups should be ceiling(length(x) / l)")
  expect_true(all(sapply(split_result, length) <= l), info = "Each group should not exceed the specified length")
})

test_that("split_equal2 returns the original vector when l is greater than length of vector", {
  x <- 1:10
  l <- 20
  split_result <- split_equal2(x, l)
  expect_equal(length(split_result), 1, info = "Only one group should be returned when l is greater than length of vector")
  expect_equal(split_result[[1]], x, info = "The single group should contain the original vector")
})

test_that("split_equal2 handles empty vector", {
  x <- integer(0)
  l <- 3
  split_result <- split_equal2(x, l)
  expect_equal(length(split_result), 0, info = "No groups should be returned for an empty vector")
})

test_that("split_equal2 handles vector where length is exactly multiple of l", {
  x <- 1:12
  l <- 4
  split_result <- split_equal2(x, l)
  expect_equal(length(split_result), length(x) / l, info = "Number of groups should be length(x) / l")
  expect_true(all(sapply(split_result, length) == l), info = "Each group should have the specified length")
})

test_that("split_equal2 handles vector with length less than l", {
  x <- 1:2
  l <- 5
  split_result <- split_equal2(x, l)
  expect_equal(length(split_result), 1, info = "Only one group should be returned when vector length is less than l")
  expect_equal(split_result[[1]], x, info = "The single group should contain the original vector")
})

test_that("split_equal splits vector into equal parts correctly when n equals length of vector", {
  x <- 1:10
  n <- 10
  split_result <- split_equal(x, n)
  expect_equal(length(split_result), n, info = "Length of result should be equal to n")
  expect_true(all(sapply(split_result, length) == 1), info = "Each group should contain exactly one element")
})

test_that("split_equal2 splits vector into parts of specified length correctly when l equals 1", {
  x <- 1:10
  l <- 1
  split_result <- split_equal2(x, l)
  expect_equal(length(split_result), length(x), info = "Number of groups should be equal to length of vector")
  expect_true(all(sapply(split_result, length) == 1), info = "Each group should contain exactly one element")
})
