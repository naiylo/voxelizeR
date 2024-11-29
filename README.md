# voxelizer

The voxelizer package offers advanced functionalities for voxelizing LiDAR point cloud data.

## License

This package is licensed under the GPL-3.0-or-later License.

SPDX-License-Identifier: GPL-3.0-or-later

Copyright 2024 Helmholtz Centre Potsdam - GFZ German Research Centre for Geosciences
Copyright 2024 Benjamin Brede

### Overview

The `voxelizeR` package provides functionalities for voxelizing LiDAR point cloud data. It implements ray-tracing algorithms to compute metrics such as Leaf Area Density (LAD) and perform occlusion mapping within defined spatial boundaries. It is developed in an cooperation of the "Deutsche GeoForschungsZentrum" and the "University of Potsdam".

### Installation

You can install the development version of `voxelizeR` from GitHub using `devtools`:

``` r
install.packages("devtools")
devtools::install_github("naiylo/voxelizeR")
```

### Features

The `voxelizeR` package includes the following key functionalities:

-   **Voxelization**: Convert LiDAR point clouds into voxel representations.
-   **Ray-Tracing**: Efficiently trace rays through defined spatial volumes.
-   **Leaf Area Density (LAD)**: Estimate LAD using various algorithms.
-   **Occlusion Mapping**: Analyze occlusions within the point cloud.

### Functions

The `voxelizeR` package is equipped with a variety of functions that provide users with the tools necessary to perform voxelization, ray-tracing, and occlusion mapping on LiDAR point cloud data. Each function is designed to integrate seamlessly with the R environment, making it easy to incorporate these advanced processes into your existing workflows.

In this section, you will find a detailed overview of each function included in the `voxelizeR` package. For every function, we provide the following information:

-   **Function Name and Purpose:** A brief description of the function and its intended use.
-   **Parameters:** A list of arguments that the function accepts, along with a description of each parameter and its expected input.
-   **Return Value:** A description of the output produced by the function.
-   **Usage Example:** A concise example that demonstrates how to use the function in practice.

This structured approach ensures that you can quickly find the information you need and apply it to your data analysis tasks.

### Example

The `voxelizeR` package is designed to be intuitive and user-friendly, offering a streamlined experience for users of all levels. To help you get started, we’ve included a series of practical examples that demonstrate the package's functionality in real-world scenarios. These examples will guide you through the core processes, showcasing how to apply `voxelizeR` to achieve meaningful results. For more detailed instructions and examples specific to individual functions, please refer to the Documentation section.

## Getting Started with voxelizeR

Before diving into the examples, you’ll need some data to work with. Fortunately, `voxelizeR` comes with a set of mock data that you can use to familiarize yourself with the package's features. This mock data is conveniently located in the `./inst/extdata` directory of the package.

The functions within `voxelizeR` are designed to work with `.laz` and `.traj` files, which are common formats for point cloud and trajectory data, respectively. Additionally, for normalization processes, a Digital Elevation Model (DEM) file of the relevant area is required.

## Loading and Preparing Data

To start, let's load a sample `.laz` file and a corresponding DEM file. This data will be used to demonstrate how to voxelize a point cloud and perform basic analyses.

``` r
# Load required packages
library(voxelizeR)
library(lidR)
library(pracma)
library(dplyr)
library(data.table)
library(sf)
```

First, define the paths to the data (in our example the `.laz`and `.traj` file), and read the data into R. Ensure the Coordinate Reference System (CRS) is set correctly for the LAS object.

``` r
# Define paths to sample data
data_file <- system.file("extdata", "H7_LS_F2_H20_200901-120129.laz", package = "voxelizeR")
traj_file <- system.file("extdata", "H7_LS_F2_H20_200901-120129.traj", package = "voxelizeR")

# Read in LAS data
las <- readLAS(data_file)

# Ensure the CRS of the LAS object
epsg(las) <- 32631
```
Next, create a trajectory for the LAS object by reading the .traj file and renaming relevant columns. The trajectory data will be used to compute rays between the point cloud data and the trajectory.
``` r
# Read and format trajectory data
colnames <- c('gpstime', 'roll', 'pitch', 'yaw', 'Xorigin', 'Yorigin', 'Zorigin')
traj <- fread(traj_file, col.names = colnames) %>%
  select(gpstime, Xorigin, Yorigin, Zorigin) %>%
  rename(Xtraj = Xorigin,
         Ytraj = Yorigin,
         Ztraj = Zorigin) %>%
  mutate(gpstime = as.numeric(gpstime),
         Xtraj = as.numeric(Xtraj),
         Ytraj = as.numeric(Ytraj),
         Ztraj = as.numeric(Ztraj))
```

## Voxelization of Point Cloud Data

Voxelization is the process of converting the 3D space into a grid of cubes `voxels`, where each cube contains point cloud data. In this example, we will perform voxelization on a subset of the LAS data.

``` r
# Create a subset of the LAS data
laz <- las[1:10,]

# Compute rays from LAS points to the trajectory
rays <- las2rays(laz, traj)

# Prepare tiles for voxelization, based on a defined area of interest (AOI)
tiles <- prepare_tiles(c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680),
                       res = c(x = 1, y = 1),
                       tilesize = c(20, 20),
                       crs = 32631)

# Define the area of interest (AOI) based on tiles
aoi <- st_bbox(tiles) %>%
  as.numeric() %>%
  setNames(names(st_bbox(tiles)))

# Define the z-range for voxelization
zrange <- c(50, 55) %>%
  setNames(c("zmin", "zmax"))

# Define the voxel resolution
res <- c(x = 1, y = 1, z = 1)

# Perform voxelization
vox <- voxelize(
  rays = rays,
  tiles = tiles,
  zrange = zrange,
  res = res,
  ac_single = 0.001,
  voxel_mode = "OCC",
  process_tiles_parallel = 1,
  process_order_tiles = "random"
)
```
## Visualizing the result

Once the voxelization process is complete, we can visualize the voxelized point cloud data directly within the R Markdown document.
``` r
# Plot voxelized data
plot(vox)
```

## Further Exploration

These examples provide a basic overview of what you can achieve with `voxelizeR.` To explore additional functionalities, such as advanced filtering, data export, or integrating multiple datasets, we encourage you to visit the Documentation section. There, you will find more comprehensive examples, along with detailed descriptions of each function’s parameters and outputs.

### Contributing

Contributions to `voxelizeR` are welcome. You can contribute by forking the repository, making your changes, and submitting a pull request. For major changes, please open an issue first to discuss what you would like to change.

### License

This project is licensed under the MIT License - see the [LICENSE](https://github.com/naiylo/voxelizeR/blob/master/LICENSE) file for details.
