test_that("Initialization of object Rays works.", {
  # Create a Rays object
  rays_obj <- new("Rays")
  expect_s4_class(rays_obj, "Rays")
})

# More tests to come after asking Benjamin what is needed
