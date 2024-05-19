test_that("Vox object can be initialized", {
  vox <- new("Vox")
  expect_true(is(vox, "Vox"))
})

test_that("Show method works for Vox", {
  vox <- new("Vox")
  expect_output(print(vox), "class            : Vox")
})

test_that("Head method works for Vox", {
  vox <- new("Vox")
  vox@data <- data.frame(Xvoxel = 1:5, Yvoxel = 1:5, Zvoxel = 1:5)
  expect_equal(nrow(head.Vox(vox)), 5)
})
