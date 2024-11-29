# SPDX-FileCopyrightText: 2024 Helmholtz Centre Potsdam - GFZ German Research Centre for Geosciences
# SPDX-FileCopyrightText: 2024 Benjamin Brede
# SPDX-License-Identifier: GPL-3.0-or-later

utils::globalVariables(c(
  "CodeOrigin", "OriginOutside", "CodeHit", "CodeOut", "X", "Xorigin",
  "D", "Yorigin", "Y", "Zorigin", "Z", "Xclip", "Yclip", "Zclip",
  "XYZisHit", "ReturnNumber", "NumberOfReturns", "lx", "ly", "lz", "d"
))

#' Fast clipping of rays according to voi
#'
#' @importFrom data.table as.data.table
#' @importFrom methods new
#' @importFrom sf st_bbox st_crs
#'
#' @author Benjamin Brede
#'
#' @details
#' Implementation of the Cohen-Sutherland algorithm: https://en.wikipedia.org/wiki/Cohen%E2%80%93Sutherland_algorithm
#'
#' @param rays An object of class Rays.
#' @param aoi A numeric vector defining the area of interest with dimensions xmin, ymin, xmax, ymax.
#' @param zrange A numeric vector defining the z-range with dimensions zmin, zmax.
#' @param buffer numeric of length 3. Buffer to add around voi to avoid edge effects in other processing. Three dimensions as las. Same unit as las
#' @param ... Additional arguments passed to other methods or functions.
#'
#' @return An object of class 'Rays' that has been clipped according to the specified parameters.
#'
#' @examples
#' # Load required packages
#' library(data.table)
#' library(lidR)
#' library(sf)
#' library(dplyr)
#'
#' # Initialize LAS object
#' data_file <- system.file("extdata", "H7_LS_F2_H20_200901-120129.laz", package = "voxelizer")
#' las <- readLAS(data_file)
#' epsg(las) <- 32631
#' laz <- las[1:100]
#'
#' # Create trajectory for the object
#' traj_file <- system.file("extdata", "H7_LS_F2_H20_200901-120129.traj", package = "voxelizer")
#' traj <- fread(traj_file, col.names = c('gpstime', 'roll', 'pitch',
#'                                        'yaw', 'Xorigin', 'Yorigin',
#'                                        'Zorigin')) %>%
#'   select(gpstime, Xorigin, Yorigin, Zorigin) %>%
#'   rename(Xtraj = Xorigin, Ytraj = Yorigin, Ztraj = Zorigin)
#'
#' # Create Rays object
#' rays <- las2rays(laz, traj)
#'
#' # Define area of interest (AOI) and z-range
#' aoi <- c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680)
#' zrange <- c(zmin = 50, zmax = 55)
#'
#' # Example 1: Clip rays with no buffer
#' clipped_rays_no_buffer <- clip_rays(rays, aoi, zrange, buffer = 0)
#' print(clipped_rays_no_buffer)
#'
#' # Example 2: Clip rays with buffer
#' buffer <- c(10, 10, 1)
#' clipped_rays_with_buffer <- clip_rays(rays, aoi, zrange, buffer)
#' print(clipped_rays_with_buffer)
#'
#' # Example 3: Clip rays with occlusion extension
#' extension <- 1000
#' clipped_rays_with_occlusion <- clip_rays(rays,
#'                                          aoi,
#'                                          zrange,
#'                                          buffer = 0,
#'                                          occ_extend = TRUE,
#'                                          extension)
#' print(clipped_rays_with_occlusion)
#'
#' # Example 4: Clip rays with empty input
#' empty_rays <- new("Rays")
#' clipped_empty_rays <- clip_rays(empty_rays, aoi, zrange, buffer = 0)
#' print(clipped_empty_rays)
#'
#' @export

setGeneric("clip_rays",
           function(rays, aoi, zrange, buffer, ...) standardGeneric("clip_rays"))

#' Clip Rays Within Specified Area of Interest and Z-Range
#'
#' @param rays An object of class 'Rays' representing the rays to be clipped.
#' @param aoi A numeric vector defining the area of interest with dimensions xmin, ymin, xmax, ymax.
#' @param zrange A numeric vector defining the z-range with dimensions zmin, zmax.
#' @param buffer A numeric value or vector specifying the buffer distance to extend around the AOI in all directions.
#'               If a single value is provided, it is used for all directions; if a vector is provided, it should have
#'               three elements corresponding to the x, y, and z directions, respectively.
#' @param occ_extend A logical value indicating whether to extend rays for occlusion processing. Defaults to FALSE.
#' @param extension A numeric value specifying the distance to extend rays during occlusion processing.
#'                  This parameter is only used if 'occ_extend' is TRUE. Defaults to 1000.
#'
#' @return An object of class 'Rays' that has been clipped according to the specified parameters.
#'
#' @export

setMethod("clip_rays",
          signature = c("Rays", "numeric", "numeric", "numeric"),
          definition = function(rays, aoi, zrange, buffer = 0, occ_extend = FALSE, extension = 1000) {

            # force  names
            if (!all(sapply(c("xmin", "ymin", "xmax", "ymax"), function(n) n %in% names(aoi))))
              stop("aoi misses dimension(s)")

            if (!all(sapply(c("zmin", "zmax"), function(n) n %in% names(zrange))))
              stop("zrange misses dimension(s)")

            if (length(buffer) == 1)
              buffer <- rep(buffer, 3)

            voi <- c(aoi["xmin"], aoi["ymin"], zrange["zmin"],
                     aoi["xmax"], aoi["ymax"], zrange["zmax"])

            clipped_dt <- clip_rays_df(pc = rays@data, voi = voi, buffer = unname(buffer)) %>%
              as.data.table()

            # occlusion processing: extend pulses
            if (occ_extend) {

              clipped_dt <- data.table::copy(rays@data) %>%
                # only last returns
                filter(ReturnNumber == NumberOfReturns) %>%
                mutate(lx = X - Xorigin,
                       ly = Y - Yorigin,
                       lz = Z - Zorigin,
                       d = extension / sqrt(lx ^ 2 + ly ^ 2 + lz ^ 2),
                       Xorigin = X,
                       Yorigin = Y,
                       Zorigin = Z,
                       X = d * lx + X,
                       Y = d * ly + Y,
                       Z = d * lz + Z) %>%
                select(-lx, -ly, -lz, -d) %>%
                mutate(IsOccluded = 1,
                       XYZisHit = 0) %>%
                # # treat occluded pulses separate from regular ones
                bind_rows(clipped_dt)
            }


            if (nrow(clipped_dt) == 0) {
              # return empty rays object
              clipped_rays <- new("Rays") %>%
                st_set_crs(st_crs(rays))

            } else {

              header = LASheader(clipped_dt)
              header$`X scale factor` <- rays@header$`X scale factor`
              header$`Y scale factor` <- rays@header$`Y scale factor`
              header$`Z scale factor` <- rays@header$`Z scale factor`
              header@PHB[["Global Encoding"]][["WKT"]] <- TRUE
              header@PHB[["Version Minor"]] <- 4L
              header@PHB[["Point Data Format ID"]] <- 6L
              clipped_rays <- LAS(clipped_dt, header)
              las_quantize(clipped_rays)
              st_crs(clipped_rays) <- st_crs(rays)
              class(clipped_rays) <- "Rays"
            }

            return(clipped_rays)
          })


#' Clip Rays with 'sf' Object for Area of Interest and Numeric Z-Range
#'
#' @author Benjamin Brede
#'
#' @param rays An object of class 'Rays' representing the rays to be clipped.
#' @param aoi An 'sf' object defining the area of interest (AOI).
#' @param zrange A numeric vector with named elements 'zmin' and 'zmax' defining the vertical range of interest.
#' @param buffer A numeric value or vector specifying the buffer distance to extend around the AOI in all directions.
#'               If a single value is provided, it is used for all directions; if a vector is provided, it should have
#'               three elements corresponding to the x, y, and z directions, respectively.
#' @param occ_extend A logical value indicating whether to extend rays for occlusion processing. Defaults to FALSE.
#' @param extension A numeric value specifying the distance to extend rays during occlusion processing.
#'
#' @return An object of class 'Rays' that has been clipped according to the specified parameters.
#'
#' @export

setMethod("clip_rays",
          signature = c("Rays", "sf", "numeric", "numeric"),
          definition = function(rays, aoi, zrange, buffer, occ_extend = FALSE, extension = 1000) {

            bb <- st_bbox(aoi)
            aoi_num <- c(xmin = bb["xmin"], ymin = bb["ymin"],
                         xmax = bb["xmax"], ymax = bb["ymax"])

            clip_rays(rays, aoi = aoi_num, zrange = zrange, buffer = buffer, occ_extend, extension)
          })

#' Method for clipping rays with 'sf' object for area of interest and numeric z-range
#'
#' @author Benjamin Brede
#'
#' @param pc data.frame or data.table with at least columns X, Y, Z, Xorigin, Yorigin, Zorigin
#' @param voi named numeric volume of interest with c(xmin, ymin, zmin, xmax, ymax, zmax)
#' @param buffer buffer to apply to voi in directions c(x, y, z)
#'
#' @return An object of class 'Rays' that has been clipped according to the specified parameters.
#'
#' @noRd

clip_rays_df <- function(pc, voi, buffer = rep(0, 3)) {

  # Ensure 'pc' has the required columns
  required_columns <- c("X", "Y", "Z", "Xorigin", "Yorigin", "Zorigin")
  missing_columns <- setdiff(required_columns, names(pc))
  if (length(missing_columns) > 0) {
    stop(paste("The input 'pc' is missing the following required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Subtract buffer from min coordinates, add to max coordinates
  voi <- voi + c(-buffer, buffer)

  INSIDE <- 0L  # 000000
  LEFT <- 1L    # 000001
  RIGHT <- 2L   # 000010
  FRONT <- 4L   # 000100
  BACK <- 8L    # 001000
  BOTTOM <- 16L # 010000
  TOP <- 32L    # 100000

  # On boundary = inside
  compute_code <- function(x, y, z) {
    code <- integer(length = length(x))
    code[x < voi["xmin"]] <- LEFT
    code[x > voi["xmax"]] <- RIGHT
    code[y < voi["ymin"]] <- bitwOr(code[y < voi["ymin"]], FRONT)
    code[y > voi["ymax"]] <- bitwOr(code[y > voi["ymax"]], BACK)
    code[z < voi["zmin"]] <- bitwOr(code[z < voi["zmin"]], BOTTOM)
    code[z > voi["zmax"]] <- bitwOr(code[z > voi["zmax"]], TOP)
    code
  }

  pc_trivial_accept <- data.frame(matrix(ncol = ncol(pc), nrow = 0, dimnames = list(NULL, names(pc))))

  code_hit <- compute_code(pc$X, pc$Y, pc$Z)
  code_origin <- compute_code(pc$Xorigin, pc$Yorigin, pc$Zorigin)
  trivial_accept <- bitwOr(code_origin, code_hit) == 0
  trivial_reject <- bitwAnd(code_origin, code_hit) != 0

  is_trivial <- trivial_accept | trivial_reject

  while (nrow(pc) != 0) {
    pc_trivial_accept <- rbind(pc_trivial_accept, pc[trivial_accept,])

    if (all(is_trivial)) {
      pc <- data.frame(matrix(ncol = ncol(pc), nrow = 0, dimnames = list(NULL, names(pc))))
    } else {
      pc <- pc[!is_trivial,] %>%
        mutate(
          CodeOrigin = code_origin[!is_trivial],
          CodeHit = code_hit[!is_trivial],
          OriginOutside = CodeOrigin != 0,
          CodeOut = if_else(OriginOutside, CodeOrigin, CodeHit)
        ) %>%
        group_split(CodeOut) %>%
        lapply(FUN = function(df) {
          if (nrow(df) == 0) {
            NULL
          } else if (bitwAnd(df$CodeOut[1], LEFT)) {
            df %>%
              mutate(D = (voi["xmin"] - X) / (Xorigin - X),
                     Xclip = voi["xmin"],
                     Yclip = D * (Yorigin - Y) + Y,
                     Zclip = D * (Zorigin - Z) + Z)
          } else if (bitwAnd(df$CodeOut[1], RIGHT)) {
            df %>%
              mutate(D = (voi["xmax"] - X) / (Xorigin - X),
                     Xclip = voi["xmax"],
                     Yclip = D * (Yorigin - Y) + Y,
                     Zclip = D * (Zorigin - Z) + Z)
          } else if (bitwAnd(df$CodeOut[1], FRONT)) {
            df %>%
              mutate(D = (voi["ymin"] - Y) / (Yorigin - Y),
                     Xclip = D * (Xorigin - X) + X,
                     Yclip = voi["ymin"],
                     Zclip = D * (Zorigin - Z) + Z)
          } else if (bitwAnd(df$CodeOut[1], BACK)) {
            df %>%
              mutate(D = (voi["ymax"] - Y) / (Yorigin - Y),
                     Xclip = D * (Xorigin - X) + X,
                     Yclip = voi["ymax"],
                     Zclip = D * (Zorigin - Z) + Z)
          } else if (bitwAnd(df$CodeOut[1], BOTTOM)) {
            df %>%
              mutate(D = (voi["zmin"] - Z) / (Zorigin - Z),
                     Xclip = D * (Xorigin - X) + X,
                     Yclip = D * (Yorigin - Y) + Y,
                     Zclip = voi["zmin"])
          } else {
            df %>%
              mutate(D = (voi["zmax"] - Z) / (Zorigin - Z),
                     Xclip = D * (Xorigin - X) + X,
                     Yclip = D * (Yorigin - Y) + Y,
                     Zclip = voi["zmax"])
          }
        }) %>%
        bind_rows() %>%
        filter(D != 0) %>%
        mutate(
          X = ifelse(OriginOutside, X, Xclip),
          Y = ifelse(OriginOutside, Y, Yclip),
          Z = ifelse(OriginOutside, Z, Zclip),
          Xorigin = ifelse(OriginOutside, Xclip, Xorigin),
          Yorigin = ifelse(OriginOutside, Yclip, Yorigin),
          Zorigin = ifelse(OriginOutside, Zclip, Zorigin),
          XYZisHit = OriginOutside & XYZisHit
        ) %>%
        select(all_of(names(pc)))

      code_hit <- compute_code(pc$X, pc$Y, pc$Z)
      code_origin <- compute_code(pc$Xorigin, pc$Yorigin, pc$Zorigin)

      trivial_accept <- bitwOr(code_origin, code_hit) == 0
      trivial_reject <- bitwAnd(code_origin, code_hit) != 0

      is_trivial <- trivial_accept | trivial_reject
    }
  }

  pc_trivial_accept
}

