#' Normalize voxel heights by terrain
#'
#' @import terra
#' @import dplyr
#'
#' @author Benjamin Brede
#'
#' @description
#' This function normalizes the heights of voxels based on a digital elevation model (DEM).
#' It resamples the DEM to match the resolution and extent of the voxels and then adjusts
#' the voxel heights accordingly.
#'
#' @name normalize_height
#' @aliases normalize_height,normalize_height-methods
#' @rdname normalize_height-methods
#' @docType methods
#' @usage normalize_height(vox, dem, ...)
#'
#' @param vox An object of class 'Vox' representing the voxel data.
#' @param dem An object of class 'SpatRaster' representing the digital elevation model.
#' @param ... Additional arguments passed to other methods or functions.
#'
#' @return An object of class 'Vox' with normalized heights.
#'
#' @export

setGeneric("normalize_height",
           function(vox,
                    dem, ...) standardGeneric("normalize_height"))

#' @rdname normalize_height-methods

setMethod("normalize_height", signature = c("Vox", "SpatRaster"), definition = function(vox, dem) {

  # Ensure that the CRS is correctly set and compatible
  crs_vox <- vox@crs
  crs_dem <- crs(dem)

  if (crs_vox != crs(dem)) {
    print("Not equal CRS")
  }

  ## resample dem to Vox resolution and extent
  tmpl <- rast(xmin = vox@extent["xmin"], ymin = vox@extent["ymin"],
               xmax = vox@extent["xmax"], ymax = vox@extent["ymax"],
               resolution = vox@resolution[c("x", "y")],
               crs = vox@crs)

  dem_resample <- resample(dem, tmpl, method = "bilinear")

  # dem as data.frame
  dem_df <- as.data.frame(crds(dem_resample)) %>%
    rename(Xvoxel = .data$x,
           Yvoxel = .data$y) %>%
    mutate(Xvoxel = bin_int(.data$Xvoxel, res = vox@resolution["x"], origin = vox@extent["xmin"]) - 1,
           Yvoxel = bin_int(.data$Yvoxel, res = vox@resolution["y"], origin = vox@extent["ymin"]) - 1,
           DEMheight = bin_int(as.data.frame(dem_resample)[,1], res = vox@resolution["z"], origin = vox@extent["zmin"]))

  vox@data <- vox@data %>%
    inner_join(dem_df, by = c('Xvoxel', 'Yvoxel'), relationship = "many-to-many") %>%
    mutate(Zvoxel = .data$Zvoxel - .data$DEMheight)

  vox@extent["zmin"] <- 0
  vox@extent["zmax"] <- max(vox@data$Zvoxel) * vox@resolution["z"]

  vox@height_normalized <- TRUE

  return(vox)
  })
