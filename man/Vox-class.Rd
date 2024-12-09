% SPDX-FileCopyrightText: 2024 Helmholtz Centre Potsdam - GFZ German Research Centre for Geosciences
% SPDX-FileCopyrightText: 2024 Benjamin Brede
% SPDX-License-Identifier: GPL-3.0-or-later

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class-Vox.R
\docType{class}
\name{Vox-class}
\alias{Vox-class}
\title{An S4 class to represent voxels}
\description{
Class Vox is the representation of a 3D grid of regularly spaced volumes, typically produced by voxelization of Rays object.
}
\section{Slots}{

\describe{
\item{\code{data}}{Voxel data with XYZvoxel columns and attributes (depend on mode)}

\item{\code{extent}}{Voxel grid extent in vox crs. (xmin, ymin, zmin, ymin, ymax, zmax)}

\item{\code{resolution}}{Voxel resolution in in vox crs. c(x, y, z)}

\item{\code{mode}}{Voxel mode: LAD (Leaf Area Density metrics) or OCC (occlusion metrics)}

\item{\code{height_normalized}}{Is Zvoxel height normalized?}

\item{\code{crs}}{CRS of Vox as \link{st_crs}}
}}

\examples{
# Load required packages
library(lidR)
library(pracma)
library(dplyr)
library(data.table)
library(sf)

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

# Check slot values
print(vox@mode)
print(vox@height_normalized)
print(vox@crs)
print(vox@extent)
print(vox@resolution)

}
\author{
Benjamin Brede
}
