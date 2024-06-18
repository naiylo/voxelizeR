# Initialize everything we need

# Create an S4 LAS object
data_file <- system.file("extdata","H7_LS_F2_H20_200901-120129.laz", package = "voxelizer")
las <- readLAS(data_file)
epsg(las) <- 32631
laz <- las[1:100]
# Create trajectory for the object
traj_file <- system.file("extdata","H7_LS_F2_H20_200901-120129.traj", package = "voxelizer")
traj <- fread(traj_file, col.names = c('gpstime', 'roll', 'pitch', 'yaw', 'Xorigin', 'Yorigin', 'Zorigin')) %>%
  select(gpstime, Xorigin, Yorigin, Zorigin) %>%
  rename(Xtraj = Xorigin,
         Ytraj = Yorigin,
         Ztraj = Zorigin)
# Create an S4 Rays object
rays <- las2rays(laz, traj)
# Prepare tiles
tiles <- prepare_tiles(c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680),
                       res = c(x = 1, y = 1),
                       tilesize = c(20, 20),
                       crs = 32631)
# Prepare aoi
aoi <- st_bbox(tiles) %>%
  as.numeric() %>%
  setNames(names(st_bbox(tiles)))
# Prepare zrange
zrange <- c(0, 100) %>%
  setNames(c("zmin", "zmax"))
# Prepare res
res = c(x = 1, y = 1, z = 1)

vox <- suppressWarnings(
  voxelize(
    rays = rays,
    tiles = tiles,
    zrange = zrange,
    res = res,
    ac_single = 0.001,
    voxel_mode = "OCC",
    process_tiles_parallel = 1,
    process_order_tiles = "random"
  )
)

# Prepare mock dem
dem <- rast(nrows=100, ncols=100)
values(dem) <- runif(ncell(dem), min = 0, max = 100) # Random heights between 0 and 10
raster::crs(dem) <- "EPSG:32631"

normalized_vox <- suppressWarnings(normalize_voxel_height(vox, dem))

#result <- vox2sds(normalized_vox)

test_that("vox2sds works correctly and transforms a vox to a SpatRasterDataset", {
  #expect_s3_class(result, "SpatRasterDataset")
})

test_that("Check the dimensions of the result", {
  #expect_equal(dim(result), c(5, 5, 5))
})

test_that("Check the resolution of the result", {
  #expect_equal(dim(result), c(1, 1))
})

test_that("Check the extent of the result", {
  #expect_equal(extent(result), c(0, 5, 0, 5))
})

test_that("vox2sds keeps the resolution", {
  #expect_equal(terra::units(result), c("count", "count", rep("m", 5), "m2", rep("m-1", 7)))
})
