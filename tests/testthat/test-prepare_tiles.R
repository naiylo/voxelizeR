# Assuming you have a sample AOI (Area of Interest) as an sf object for testing
sample_aoi <- st_as_sf(data.frame(
  id = 1:4,
  x = c(0, 10, 10, 0),
  y = c(0, 0, 10, 10),
  stringsAsFactors = FALSE
), coords = c("x", "y"), crs = 4326)

test_that("prepare_tiles returns an sf object", {
  result <- prepare_tiles(sample_aoi, res = c(1, 1), tilesize = c(5, 5))
  expect_s3_class(result, "sf")
})

test_that("prepare_tiles handles sfc input", {
  sfc_aoi <- st_geometry(sample_aoi)
  result <- prepare_tiles(sfc_aoi, res = c(1, 1), tilesize = c(5, 5))
  expect_s3_class(result, "sf")
})

test_that("prepare_tiles handles bbox input", {
  bbox_aoi <- st_bbox(sample_aoi)
  result <- prepare_tiles(bbox_aoi, res = c(1, 1), tilesize = c(5, 5), crs = st_crs(sample_aoi))
  expect_s3_class(result, "sf")
})

test_that("prepare_tiles handles numeric input", {
  numeric_aoi <- st_bbox(sample_aoi)
  result <- prepare_tiles(numeric_aoi, res = c(1, 1), tilesize = c(5, 5), crs = st_crs(sample_aoi))
  expect_s3_class(result, "sf")
})

test_that("prepare_tiles creates the correct number of tiles", {
  result <- prepare_tiles(sample_aoi, res = c(1, 1), tilesize = c(5, 5))
  expected_n_tiles <- ceiling((st_bbox(sample_aoi)[c("xmax", "ymax")] -
                                 floor(st_bbox(sample_aoi)[c("xmin", "ymin")])) / c(5, 5))
  expect_equal(dim(result)[1], prod(expected_n_tiles))
})

# TODO maybe add tests for own data -> not sure how to get the aoi from the data
