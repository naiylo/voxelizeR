utils::globalVariables(c(
  "DEMheight"
))


#' Normalize voxel heights by terrain
#'
#' @importFrom raster ncell values<-
#' @importFrom terra rast resample crds
#' @importFrom dplyr rename mutate inner_join
#'
#' @author Benjamin Brede
#'
#' @description
#' This function normalizes the heights of voxels based on a digital elevation model (DEM).
#' It resamples the DEM to match the resolution and extent of the voxels and then adjusts
#' the voxel heights accordingly.
#'
#' @name normalize_voxel_height
#' @aliases normalize_voxel_height,normalize_voxel_height-methods
#' @rdname normalize_voxel_height-methods
#' @docType methods
#'
#' @param vox An object of class 'Vox' representing the voxel data.
#' @param dem An object of class 'SpatRaster' representing the digital elevation model.
#' @param ... Additional arguments passed to other methods or functions.
#'
#' @return An object of class 'Vox' with normalized heights.
#'
#' @export
#'
normalize_voxel_height <- function(vox, dem, ...) {

  if (!inherits(vox, "Vox")) {
    stop("Input 'vox' must be an object of class 'Vox'")
  }
  if (!inherits(dem, "SpatRaster")) {
    stop("Input 'dem' must be an object of class 'SpatRaster'")
  }

  ## resample dem to Vox resolution and extent
  tmpl <- rast(xmin = vox@extent["xmin"], ymin = vox@extent["ymin"],
               xmax = vox@extent["xmax"], ymax = vox@extent["ymax"],
               resolution = vox@resolution[c("x", "y")],
               crs = vox@crs)

  dem_resample <- resample(dem, tmpl, method = "bilinear")

  # dem as data.frame
  dem_df <- as.data.frame(crds(dem_resample)) %>%
    rename(Xvoxel = x,
           Yvoxel = y) %>%
    mutate(Xvoxel = bin_int(Xvoxel, res = vox@resolution["x"], origin = vox@extent["xmin"]) - 1,
           Yvoxel = bin_int(Yvoxel, res = vox@resolution["y"], origin = vox@extent["ymin"]) - 1,
           DEMheight = bin_int(as.data.frame(dem_resample)[,1], res = vox@resolution["z"], origin = vox@extent["zmin"]))

  vox@data <- vox@data %>%
    inner_join(dem_df, by = c('Xvoxel', 'Yvoxel'), relationship = "many-to-many") %>%
    mutate(Zvoxel = Zvoxel - DEMheight)

  vox@extent["zmin"] <- 0
  vox@extent["zmax"] <- max(vox@data$Zvoxel, na.rm = TRUE) * vox@resolution["z"]

  vox@height_normalized <- TRUE

  vox
}
