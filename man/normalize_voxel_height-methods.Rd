% SPDX-FileCopyrightText: 2024 Helmholtz Centre Potsdam - GFZ German Research Centre for Geosciences
% SPDX-FileCopyrightText: 2024 Benjamin Brede
% SPDX-License-Identifier: GPL-3.0-or-later

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize_voxel_height.R
\docType{methods}
\name{normalize_voxel_height}
\alias{normalize_voxel_height}
\alias{normalize_voxel_height,normalize_voxel_height-methods}
\title{Normalize voxel heights by terrain}
\usage{
normalize_voxel_height(vox, dem, ...)
}
\arguments{
\item{vox}{An object of class Vox representing the voxel data.}

\item{dem}{An object of class SpatRaster representing the digital elevation model.}

\item{...}{Additional arguments passed to other methods or functions.}
}
\value{
An object of class Vox with normalized heights.
}
\description{
This function normalizes the heights of voxels based on a digital elevation model (DEM).
It resamples the DEM to match the resolution and extent of the voxels and then adjusts
the voxel heights accordingly.
}
\examples{
# Load required packages
library(lidR)
library(pracma)
library(dplyr)
library(data.table)
library(sf)
library(terra)

# Read in LAS data
data_file <- system.file("extdata", "H7_LS_F2_H20_200901-120129.laz", package = "voxelizer")
las <- readLAS(data_file)

# Ensure the CRS of the LAS object
epsg(las) <- 32631

# Create a trajectory for the LAS object
traj_file <- system.file("extdata", "H7_LS_F2_H20_200901-120129.traj", package = "voxelizer")
colnames <- c('gpstime', 'roll', 'pitch', 'yaw', 'Xorigin', 'Yorigin', 'Zorigin')
traj <- fread(traj_file, col.names = colnames) \%>\%
  select(gpstime, Xorigin, Yorigin, Zorigin) \%>\%
  rename(Xtraj = Xorigin,
         Ytraj = Yorigin,
         Ztraj = Zorigin) \%>\%
  mutate(gpstime = as.numeric(gpstime),
         Xtraj = as.numeric(Xtraj),
         Ytraj = as.numeric(Ytraj),
         Ztraj = as.numeric(Ztraj))

# Create a subset of the LAS data
laz <- las[1:10,]

# Compute rays from LAS points and trajectory
rays <- las2rays(laz, traj)

# Prepare tiles
tiles <- prepare_tiles(c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680),
                       res = c(x = 1, y = 1),
                       tilesize = c(20, 20),
                       crs = 32631)

# Prepare aoi
aoi <- st_bbox(tiles) \%>\%
  as.numeric() \%>\%
  setNames(names(st_bbox(tiles)))

# Prepare zrange
zrange <- c(50, 55) \%>\%
  setNames(c("zmin", "zmax"))

# Prepare res
res <- c(x = 1, y = 1, z = 1)

# Use voxelize function
vox <- suppressWarnings(voxelize(
  rays = rays,
  tiles = tiles,
  zrange = zrange,
  res = res,
  ac_single = 0.001,
  voxel_mode = "OCC",
  process_tiles_parallel = 1,
  process_order_tiles = "random"
))

# Prepare Dem
dem_file <- system.file("extdata","UAV4LAI_DEM.tif", package = "voxelizer")
dem <- rast(dem_file)
raster::crs(dem) <- "EPSG:32631"

# Normalize voxel heights using the DEM
normalized_vox <- normalize_voxel_height(vox, dem)

# Check the results
print(normalized_vox)

}
\author{
Benjamin Brede
}
