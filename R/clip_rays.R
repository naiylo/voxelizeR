#' Fast clipping of rays according to voi
#'
#' @import dplyr
#' @import data.table
#' @importFrom methods new
#'
#' @author Benjamin Brede
#'
#' @details
#' Implementation of the Cohen-Sutherland algorithm: \url{https://en.wikipedia.org/wiki/Cohen%E2%80%93Sutherland_algorithm}
#'
#' @param rays An object of class 'Rays'.
#' @param aoi A numeric vector defining the area of interest with dimensions xmin, ymin, xmax, ymax.
#' @param zrange A numeric vector defining the z-range with dimensions zmin, zmax.
#' @param buffer numeric of length 3. Buffer to add around voi to avoid edge effects in other processing. Three dimensions as las. Same unit as las
#' @param ... Additional arguments passed to other methods or functions.
#'
#' @return An object of class 'Rays' that has been clipped according to the specified parameters.
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
                filter(.data$ReturnNumber == .data$NumberOfReturns) %>%
                # select(X, Y, Z, Xorigin, Yorigin, Zorigin) %>%
                mutate(lx = .data$X - .data$Xorigin,
                       ly = .data$Y - .data$Yorigin,
                       lz = .data$Z - .data$Zorigin,
                       d = extension / sqrt(.data$lx ^ 2 + .data$ly ^ 2 + .data$lz ^ 2),
                       Xorigin = .data$X,
                       Yorigin = .data$Y,
                       Zorigin = .data$Z,
                       X = .data$d * .data$lx + .data$X,
                       Y = .data$d * .data$ly + .data$Y,
                       Z = .data$d * .data$lz + .data$Z) %>%
                select(-.data$lx, -.data$ly, -.data$lz, -.data$d) %>%
                mutate(IsOccluded = 1,
                       XYZisHit = 0) %>%
                # # treat occluded pulses separate from regular ones
                bind_rows(clipped_dt)
            }


            if (nrow(clipped_dt) == 0) {
              # return empty rays object
              clipped_rays <- methods::new("Rays") %>%
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

  # subtract buffer from min coordinates, add to max coordinates
  voi <- voi + c(-buffer, buffer)

  INSIDE = 0L  # 000000
  LEFT = 1L    # 000001
  RIGHT = 2L   # 000010
  FRONT = 4L   # 000100
  BACK = 8L    # 001000
  BOTTOM = 16L # 010000
  TOP = 32L    # 100000

  # on boundary = inside
  compute_code <- function(x, y, z) {
    # integer initializes as 0 == INSIDE
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

  while(nrow(pc) != 0) {

    pc_trivial_accept <- rbind(pc_trivial_accept,
                               pc[trivial_accept,])

    if (all(is_trivial)) {
      pc <- data.frame(matrix(ncol = ncol(pc), nrow = 0, dimnames = list(NULL, names(pc))))

    } else {

      pc <- pc[!is_trivial,] %>%
        mutate(#CodeOrigin = bitwAnd(code_origin[!is_trivial], bitwNot(code_origin_boundary[!is_trivial])),
          #CodeHit = bitwAnd(code_hit[!is_trivial], bitwNot(code_hit_boundary[!is_trivial])),
          CodeOrigin = code_origin[!is_trivial],
          CodeHit = code_hit[!is_trivial],
          # if origin is outside the viewport; otherwise hit will be outside (otherwise it would be trivial)
          OriginOutside = .data$CodeOrigin != 0,
          # At least one endpoint is outside the clip rectangle; pick it.
          CodeOut = if_else(.data$OriginOutside, .data$CodeOrigin, .data$CodeHit)) %>%
        group_split(.data$CodeOut) %>%
        lapply(FUN = function(df) {
          if (nrow(df) == 0) {
            NULL
          } else if (bitwAnd(df$CodeOut[1], LEFT)) {
            df %>%
              mutate(D = (voi["xmin"] - .data$X) / (.data$Xorigin - .data$X),
                     Xclip = voi["xmin"],
                     Yclip = .data$D * (.data$Yorigin - .data$Y) + .data$Y,
                     Zclip = .data$D * (.data$Zorigin - .data$Z) + .data$Z)

          } else if (bitwAnd(df$CodeOut[1], RIGHT)) {
            df %>%
              mutate(D = (voi["xmax"] - .data$X) / (.data$Xorigin - .data$X),
                     Xclip = voi["xmax"],
                     Yclip = .data$D * (.data$Yorigin - .data$Y) + .data$Y,
                     Zclip = .data$D * (.data$Zorigin - .data$Z) + .data$Z)

          } else if (bitwAnd(df$CodeOut[1], FRONT)) {
            df %>%
              mutate(D = (voi["ymin"] - .data$Y) / (.data$Yorigin - .data$Y),
                     Xclip = .data$D * (.data$Xorigin - .data$X) + .data$X,
                     Yclip = voi["ymin"],
                     Zclip = .data$D * (.data$Zorigin - .data$Z) + .data$Z)

          } else if (bitwAnd(df$CodeOut[1], BACK)) {
            df %>%
              mutate(D = (voi["ymax"] - .data$Y) / (.data$Yorigin - .data$Y),
                     Xclip = .data$D * (.data$Xorigin - .data$X) + .data$X,
                     Yclip = voi["ymax"],
                     Zclip = .data$D * (.data$Zorigin - .data$Z) + .data$Z)

          } else if (bitwAnd(df$CodeOut[1], BOTTOM)) {
            df %>%
              mutate(D = (voi["zmin"] - .data$Z) / (.data$Zorigin - .data$Z),
                     Xclip = .data$D * (.data$Xorigin - .data$X) + .data$X,
                     Yclip = .data$D * (.data$Yorigin - .data$Y) + .data$Y,
                     Zclip = voi["zmin"])
          } else {
            df %>%
              mutate(D = (voi["zmax"] - .data$Z) / (.data$Zorigin - .data$Z),
                     Xclip = .data$D * (.data$Xorigin - .data$X) + .data$X,
                     Yclip = .data$D * (.data$Yorigin - .data$Y) + .data$Y,
                     Zclip = voi["zmax"])
          }
        }) %>%
        bind_rows() %>%
        # eliminate zero length rays
        filter(.data$D != 0) %>%
        # if origin was outside, hit coordinates will stay the same
        mutate(X = ifelse(.data$OriginOutside, .data$X, .data$Xclip),
               Y = ifelse(.data$OriginOutside, .data$Y, .data$Yclip),
               Z = ifelse(.data$OriginOutside, .data$Z, .data$Zclip),
               # if origin was outside, origin will become clipped coordinate
               Xorigin = ifelse(.data$OriginOutside, .data$Xclip, .data$Xorigin),
               Yorigin = ifelse(.data$OriginOutside, .data$Yclip, .data$Yorigin),
               Zorigin = ifelse(.data$OriginOutside, .data$Zclip, .data$Zorigin),
               # if origin was outside: XYZisHit will decide
               # if origin was not outside: no original hit
               XYZisHit = .data$OriginOutside & .data$XYZisHit) %>%
        # select the same columns as in pc
        select(all_of(names(pc)))

      code_hit <- compute_code(.data$pc$X, .data$pc$Y, .data$pc$Z)
      code_origin <- compute_code(pc$Xorigin, pc$Yorigin, pc$Zorigin)

      trivial_accept <- bitwOr(code_origin, code_hit) == 0
      trivial_reject <- bitwAnd(code_origin, code_hit) != 0

      is_trivial <- trivial_accept | trivial_reject
    }
  }

  pc_trivial_accept
}

