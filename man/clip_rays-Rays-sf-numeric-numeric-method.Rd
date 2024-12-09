% SPDX-FileCopyrightText: 2024 Helmholtz Centre Potsdam - GFZ German Research Centre for Geosciences
% SPDX-FileCopyrightText: 2024 Benjamin Brede
% SPDX-License-Identifier: GPL-3.0-or-later

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clip_rays.R
\name{clip_rays,Rays,sf,numeric,numeric-method}
\alias{clip_rays,Rays,sf,numeric,numeric-method}
\title{Clip Rays with 'sf' Object for Area of Interest and Numeric Z-Range}
\usage{
\S4method{clip_rays}{Rays,sf,numeric,numeric}(rays, aoi, zrange, buffer, occ_extend = FALSE, extension = 1000)
}
\arguments{
\item{rays}{An object of class 'Rays' representing the rays to be clipped.}

\item{aoi}{An 'sf' object defining the area of interest (AOI).}

\item{zrange}{A numeric vector with named elements 'zmin' and 'zmax' defining the vertical range of interest.}

\item{buffer}{A numeric value or vector specifying the buffer distance to extend around the AOI in all directions.
If a single value is provided, it is used for all directions; if a vector is provided, it should have
three elements corresponding to the x, y, and z directions, respectively.}

\item{occ_extend}{A logical value indicating whether to extend rays for occlusion processing. Defaults to FALSE.}

\item{extension}{A numeric value specifying the distance to extend rays during occlusion processing.}
}
\value{
An object of class 'Rays' that has been clipped according to the specified parameters.
}
\description{
Clip Rays with 'sf' Object for Area of Interest and Numeric Z-Range
}
\author{
Benjamin Brede
}
