# voxelizeR

[![GitHub](https://img.shields.io/github/license/naiylo/voxelizeR)](https://github.com/naiylo/voxelizeR//LICENSE) [![GitHub issues](https://img.shields.io/github/issues/naiylo/voxelizeR)](https://github.com/naiylo/voxelizeR/issues) [![GitHub last commit](https://img.shields.io/github/last-commit/naiylo/voxelizeR)](https://github.com/naiylo/voxelizeR/commits/master)

### Overview

The `voxelizeR` package provides functionalities for voxelizing LiDAR point cloud data. It implements ray-tracing algorithms to compute metrics such as Leaf Area Density (LAD) and perform occlusion mapping within defined spatial boundaries.

### Installation

You can install the development version of `voxelizeR` from GitHub using `devtools`:

``` r
install.packages("devtools")
devtools::install_github("naiylo/voxelizeR")
```

## Features

-   **Voxelization**: Convert LiDAR point clouds into voxel representations.
-   **Ray-Tracing**: Efficiently trace rays through defined spatial volumes.
-   **Leaf Area Density (LAD)**: Estimate LAD using various algorithms.
-   **Occlusion Mapping**: Analyze occlusions within the point cloud.

## Example

Here's a basic example demonstrating the usage of `voxelizeR`:

``` r
library(voxelizeR)

# Initialize everything we need

# Get LAS for the object
data_file <- system.file("extdata","H7_LS_F2_H20_200901-120129.laz", package = "voxelizer")
las <- readLAS(data_file)
epsg(las) <- 32631
laz <- las[1:100]

# Get trajectory for the object
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
zrange <- c(50, 55) %>%
  setNames(c("zmin", "zmax"))
  
# Prepare res
res = c(x = 1, y = 1, z = 1)

vox <- voxelize(
                rays = rays,
                tiles = tiles,
                zrange = zrange,
                res = res,
                ac_single = 0.001,
                voxel_mode = "OCC",
                process_tiles_parallel = 1,
                process_order_tiles = "random")
```

For detailed function documentation and examples, please refer to the package vignettes and help files.

## Contributing

Contributions to `voxelizeR` are welcome. You can contribute by forking the repository, making your changes, and submitting a pull request. For major changes, please open an issue first to discuss what you would like to change.

## License

This project is licensed under the MIT License - see the [LICENSE](https://github.com/naiylo/voxelizeR/blob/master/LICENSE) file for details.
