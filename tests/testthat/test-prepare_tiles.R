# Initialize tiles for testing
tiles <- prepare_tiles(c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680),
                       res = c(x = 1, y = 1),
                       tilesize = c(20, 20),
                       crs = 32631)

test_that("prepare_tiles returns an sf object", {
  expect_s3_class(tiles, "sf")
})

test_that("prepare_tiles handles sfc input", {
  sfc_aoi <- st_geometry(st_as_sfc(st_bbox(c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680), crs = 32631)))
  result <- prepare_tiles(sfc_aoi, res = c(1, 1), tilesize = c(20, 20))
  expect_s3_class(result, "sf")
})

test_that("prepare_tiles handles bbox input", {
  bbox_aoi <- st_bbox(c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680))
  result <- prepare_tiles(bbox_aoi, res = c(1, 1), tilesize = c(20, 20), crs = 32631)
  expect_s3_class(result, "sf")
})

test_that("prepare_tiles handles numeric input", {
  numeric_aoi <- c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680)
  result <- prepare_tiles(numeric_aoi, res = c(1, 1), tilesize = c(20, 20), crs = 32631)
  expect_s3_class(result, "sf")
})

test_that("prepare_tiles creates the correct number of tiles", {
  expected_n_tiles <- ceiling((c(xmax = 682300, ymax = 5763680) -
                                 floor(c(xmin = 682130, ymin = 5763580))) / c(20, 20))
  expect_equal(nrow(tiles), prod(expected_n_tiles))
})

test_that("prepare_tiles handles different resolutions correctly", {
  result1 <- prepare_tiles(c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680),
                           res = c(2, 2), tilesize = c(20, 20), crs = 32631)
  result2 <- prepare_tiles(c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680),
                           res = c(1, 1), tilesize = c(20, 20), crs = 32631)
  expect_true(nrow(result1) <= nrow(result2), info = "Higher resolution should result in more tiles.")
})

test_that("prepare_tiles handles different tile sizes correctly", {
  result1 <- prepare_tiles(c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680),
                           res = c(1, 1), tilesize = c(40, 40), crs = 32631)
  result2 <- prepare_tiles(c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680),
                           res = c(1, 1), tilesize = c(20, 20), crs = 32631)
  expect_true(nrow(result1) <= nrow(result2), info = "Smaller tile sizes should result in more tiles.")
})

test_that("prepare_tiles only includes tiles that intersect with the AOI", {
  bbox_aoi <- st_as_sfc(st_bbox(c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680), crs = 32631))
  result <- prepare_tiles(bbox_aoi, res = c(1, 1), tilesize = c(20, 20))
  intersects <- st_intersects(result, bbox_aoi, sparse = FALSE)
  expect_true(all(intersects), info = "All tiles should intersect with the AOI.")
})
