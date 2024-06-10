# Initialize everything we need

# Create an S4 LAS object
data_file <- system.file("extdata","H7_LS_F2_H20_200901-120129.laz", package = "voxelizer")
las <- readLAS(data_file)
epsg(las) <- 32631
laz <- las[1:100]
show(laz)
# Create trajectory for the object
traj_file <- system.file("extdata","H7_LS_F2_H20_200901-120129.traj", package = "voxelizer")
traj <- fread(traj_file, col.names = c('gpstime', 'roll', 'pitch', 'yaw', 'Xorigin', 'Yorigin', 'Zorigin')) %>%
  select(gpstime, Xorigin, Yorigin, Zorigin) %>%
  rename(Xtraj = Xorigin,
         Ytraj = Yorigin,
         Ztraj = Zorigin)
# Create an S4 Rays object
rays <- las2rays(laz,traj)
show(rays)
# Prepare tiles
tiles <- prepare_tiles(c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680),
                       res = c(x = 1, y = 1),
                       tilesize = c(20, 20),
                       crs = 32631)

aoi <- st_bbox(tiles) %>%
  as.numeric() %>%
  setNames(names(st_bbox(tiles)))


zrange <- c(50, 55) %>%
  setNames(c("zmin", "zmax"))
res = c(x = 1, y = 1, z = 0.2)


test_that("Test if voxelize works correctly", {
  #vox <- rays %>%
         #      voxelize(tiles = tiles,
        #       zrange = zrange,
       #        res = res,
      #         ac_single = 0.001,
     #          voxel_mode = "OCC",
    #           process_tiles_parallel = 1,
   #            process_order_tiles = "random")
  #show(vox)
})
