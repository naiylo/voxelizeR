#' Voxelize point cloud with ray-tracing
#'
#' @import data.table
#' @import dplyr
#' @import parallel
#' @import terra
#' @import pbmcapply
#' @importFrom methods new
#'
#' @author Benjamin Brede
#'
#' @description
#' This function voxelizes a point cloud by tracing rays through a specified volume of interest.
#' It is designed to process LiDAR data for applications such as Leaf Area Density (LAD) estimation or occlusion mapping.
#'
#' @details
#' The 'voxelize' function takes a `Rays` object, representing a point cloud with an added trajectory,
#' and a set of tiles that define the area to be processed. It then performs ray-tracing within the specified
#' vertical boundaries (`zrange`) and at a given resolution (`res`). The function supports parallel processing
#' of tiles and allows for different processing orders to optimize performance.
#'
#' @param rays An object of class Rays (LAS with added trajectory)
#' @param tiles sf. Tiles representing the area to be processed
#' @param zrange numeric. Boundaries for volume of interest in vertical dimension c(zmin, zmax)
#' @param res numeric. Resolution as single value for all three dimensions or as c(x, y, z)
#' @param ac_single numeric. Attenuation coefficient of single vegetation element (\url{https://www.mdpi.com/2072-4292/10/10/1580})
#' @param voxel_mode character. "LAD" for Leaf Area Density estimation with various estimators. "OCC" for occlusion mapping.
#' @param process_tiles_parallel numeric. Numbers of tiles to process in parallel (only on *nix systems with \link{mclapply})
#' @param process_order_tiles character. Processing order of tiles. "random" for randamized processing (typically balances processing of full and empty voxels). "seq" for sequential processing
#' @param ... Additional arguments passed to other methods or functions.
#'
#' @return Vox object.
#'
#' @export

setGeneric("voxelize",
           function(rays,
                    tiles,
                    zrange,
                    res,
                    ac_single,
                    voxel_mode,
                    process_tiles_parallel,
                    process_order_tiles, ...) standardGeneric("voxelize"))

#' Voxelize point cloud with ray-tracing
#'
#' @author Benjamin Brede
#'
#' @param rays An object of class Rays (LAS with added trajectory)
#' @param tiles sf. Tiles representing the area to be processed
#' @param zrange numeric. Boundaries for volume of interest in vertical dimension c(zmin, zmax)
#' @param res numeric. Resolution as single value for all three dimensions or as c(x, y, z)
#' @param ac_single numeric. Attenuation coefficient of single vegetation element (\url{https://www.mdpi.com/2072-4292/10/10/1580})
#' @param voxel_mode character. "LAD" for Leaf Area Density estimation with various estimators. "OCC" for occlusion mapping.
#' @param process_tiles_parallel numeric. Numbers of tiles to process in parallel (only on *nix systems with \link{mclapply})
#' @param process_order_tiles character. Processing order of tiles. "random" for randamized processing (typically balances processing of full and empty voxels). "seq" for sequential processing
#'
#' @return Vox object.
#'
#' @export

setMethod("voxelize",
          signature = c("Rays",
                        "sf"),
          definition = function(rays,
                                tiles,
                                zrange,
                                res = c(x = 1, y = 1, z = 1),
                                ac_single,
                                voxel_mode = c("LAD", "OCC"),
                                process_tiles_parallel = 1,
                                process_order_tiles = c("random", "seq")) {


            if (st_crs(rays) != st_crs(tiles))
              warning("rays and tiles need to have same CRS")

            if (!any(st_intersects(tiles, st_as_sfc(st_bbox(rays))) %>% unlist())) {
              warning("rays and tiles do not intersect")
              return(NULL)
            }

            # if (!"sf" %in% class(tiles))
            #   stop("tiles needs to be sf-object")

            if (!all(st_geometry_type(tiles) == "POLYGON"))
              stop("tiles need to be polygons")

            # all_dims needed to look for correct columns/attributes
            all_dims <- c("X", "Y", "Z")

            if (!all(sapply(c("Xtraj", "Ytraj", "Ztraj"), function(c) c %in% names(rays))))
              stop("rays misses XYZtraj columns")

            if (length(res) == 1)
              res <- rep(res, 3)

            if (is.null(names(res)))
              names(res) <- c("x", "y", "z")

            if (is.null(names(zrange)))
              names(zrange) <- c("zmin", "zmax")

            # in case zmax is not res["z"] away from zmin,
            # increase zmax so that the difference between zmin and zmax is a multiple of res["z"]
            if (diff(zrange) %% res["z"] != 0)
              zrange[2] <- ((zrange[1] + diff(zrange)) %/% res["z"] + 1) * res["z"]


            #### check tiles ####

            if (nrow(tiles) == 0 | nrow(rays) == 0) {
              # TODO correct return for empty tiles
              warning("No tiles or rays to voxelize!")
              return(NULL)
            }

            if (process_order_tiles == "random") {
              process_order <- permute(row.names(tiles))

            } else if (process_order_tiles == "seq") {
              process_order <- row.names(tiles)
            }


            #### point cloud preparation ####

            # _global refers to all tiles in original las CRS
            tiles_bb <- st_bbox(tiles)
            voi_global <- c(tiles_bb["xmin"], tiles_bb["ymin"], zrange["zmin"],
                            tiles_bb["xmax"], tiles_bb["ymax"], zrange["zmax"])
            names(voi_global) <- c(paste0(names(res), "min"), paste0(names(res), "max"))

            # checks if XYZorigin is available, otherwise copies XYZtraj-attributes, or stops
            # las <- .check_origin(las)

            # check if XYZisHit attributes is already available
            # if not assume all hits to be original (not clipped_pulses)
            # if (!"XYZisHit" %in% names(las))
            #   las@data$XYZisHit <- TRUE

            if (!"ReturnID" %in% names(rays))
              rays@data$ReturnID <- 1:nrow(rays)

            # make deep copy of pc (important in case pc is a data.table)
            # pc_scaled <- data.table::copy(las@data) %>%
            pc_scaled <- rays@data %>%
              as.data.frame() %>%
              select(.data$X, .data$Y, .data$Z, .data$Xorigin, .data$Yorigin, .data$Zorigin, .data$XYZisHit, .data$ReturnID)

            ## occlusion processing: extend pulses to zrange["zmin"] - res["z"] (subtract res to avoid edge effects)
            if (voxel_mode == "OCC") {
              pc_occlusion <- data.table::copy(rays@data) %>%
                filter(.data$ReturnNumber == .data$NumberOfReturns) %>%
                select(.data$X, .data$Y, .data$Z, .data$Xorigin, .data$Yorigin, .data$Zorigin, .data$XYZisHit, .data$ReturnID) %>%
                mutate(lx = .data$X - .data$Xorigin,
                       ly = .data$Y - .data$Yorigin,
                       lz = .data$Z - .data$Zorigin,
                       d = (.data$zrange["zmin"] - .data$res["z"] - .data$Zorigin) / .data$lz,
                       Xold = .data$X,
                       Yold = .data$Y,
                       Zold = .data$Z,
                       X = .data$d * .data$lx + .data$Xorigin,
                       Y = .data$d * .data$ly + .data$Yorigin,
                       Z = .data$zrange["zmin"] - .data$res["z"],
                       Xorigin = .data$Xold,
                       Yorigin = .data$Yold,
                       Zorigin = .data$Zold) %>%
                select(-.data$lx, -.data$ly, -.data$lz, -.data$d, -.data$Xold, -.data$Yold, -.data$Zold) %>%
                mutate(IsOccluded = TRUE,
                       XYZisHit = 0,
                       # treat occluded pulses separate from regular ones
                       ReturnID = (1:n()) + nrow(pc_scaled))

              pc_scaled <- pc_scaled %>%
                mutate(IsOccluded = FALSE) %>%
                bind_rows(pc_occlusion)

              rm(pc_occlusion)
            }

            pc_scaled <- mclapply(split_equal(1:nrow(pc_scaled), n = process_tiles_parallel), mc.cores = process_tiles_parallel, FUN = function(idx) {
              # all_of(user_columns[sapply(user_columns, function(u) u %in% names(pc) & !u %in% processing_columns)])) %>%
              # clip pulses to AOI
              # first 3 elements: min, second 3 elements: max, voi has names of voi_global
              clip_rays_df(pc_scaled[idx,], voi = voi_global, buffer = 2 * unname(res))
            }) %>%
              bind_rows() %>%
              # shift & scale
              mutate(X = (.data$X - voi_global["xmin"]) / res["x"],
                     Y = (.data$Y - voi_global["ymin"]) / res["y"],
                     Z = (.data$Z - voi_global["zmin"]) / res["z"],
                     Xorigin = (.data$Xorigin - voi_global["xmin"]) / res["x"],
                     Yorigin = (.data$Yorigin - voi_global["ymin"]) / res["y"],
                     Zorigin = (.data$Zorigin - voi_global["zmin"]) / res["z"])

            # if no pulses go through target volume defined by blocks & no empty files should be written, finish processing here
            if (nrow(pc_scaled) == 0) {
              warning("Rays is empty after clipping to VOI!")
              return(NULL)
            }

            # minZ will always be 0 in scaled space
            minZ_scaled <- 0
            # maxZ = number of resolution cells from bottom to top of voi
            maxZ_scaled <- ((voi_global["zmax"] - voi_global["zmin"]) / res["z"]) %>% unname()
            gridsZ <- minZ_scaled:maxZ_scaled

            #### process tiles ####

            vox_df = pbmclapply(X = process_order, mc.cores = process_tiles_parallel, FUN = function(ti) {

              tile_coord <- tiles[ti,] %>%
                st_coordinates()

              # scaled according to global tiles
              minX_scaled <- ((min(tile_coord[,"X"]) - voi_global["xmin"]) / res["x"]) %>% round() %>% as.integer() %>% unname()
              minY_scaled <- ((min(tile_coord[,"Y"]) - voi_global["ymin"]) / res["y"]) %>% round() %>% as.integer() %>% unname()
              maxX_scaled <- ((max(tile_coord[,"X"]) - voi_global["xmin"]) / res["x"]) %>% round() %>% as.integer() %>% unname()
              maxY_scaled <- ((max(tile_coord[,"Y"]) - voi_global["ymin"]) / res["y"]) %>% round() %>% as.integer() %>% unname()


              # clips according to block definition
              # add 1 voxel in each direction to clipping window to be certain to include outer boundary
              pc_scaled_tile <- pc_scaled %>%
                clip_rays_df(buffer = rep(2L, 3),
                             voi = c(xmin = minX_scaled,
                                     ymin = minY_scaled,
                                     zmin = minZ_scaled,
                                     xmax = maxX_scaled,
                                     ymax = maxY_scaled,
                                     zmax = maxZ_scaled))

              if (nrow(pc_scaled_tile) == 0) {

                warning(paste("Empty tile:", ti))
                NULL

              } else {

                # block grid stops (voxel boundaries in all 3 dimensions) in voxel coordinates
                # grid step points for this dimension = planes in dimension d that can potentially be crossed by hit
                gridsX <- minX_scaled:maxX_scaled
                gridsY <- minY_scaled:maxY_scaled


                #### identify crossings of pulses with voxels ####

                # iterate over dimensions (XYZ) and grid lines in each dimension
                crossings <- mapply(dim = rep(all_dims, c(length(gridsX), length(gridsY), length(gridsZ))),
                                    grid = c(gridsX, gridsY, gridsZ),
                                    SIMPLIFY = FALSE, FUN = function(dim, grid) {

                                      pc_scaled_tile %>%
                                        filter(.data[[dim]] - 2 <= grid & grid <= .data[[paste0(dim, "origin")]] |
                                                 .data[[paste0(dim, "origin")]] <= grid & grid <= .data[[dim]] + 2) %>%
                                        mutate(D = (grid - .data[[dim]]) / (.data[[paste0(dim, "origin")]] - .data[[dim]])) %>%
                                        # for cases division by zero; when origin and hit overlap in one or more dimensions
                                        filter(!is.infinite(abs(.data$D)))
                                    }) %>%
                  bind_rows() %>%
                  arrange(.data$ReturnID, .data$D) %>%
                  # the pulse entering is where it exits from the previous crossing
                  mutate(Xexit = .data$D * (.data$Xorigin - .data$X) + .data$X,
                         Yexit = .data$D * (.data$Yorigin - .data$Y) + .data$Y,
                         Zexit = .data$D * (.data$Zorigin - .data$Z) + .data$Z,
                         Xentering = data.table::shift(.data$Xexit, type = "lead"),
                         Yentering = data.table::shift(.data$Yexit, type = "lead"),
                         Zentering = data.table::shift(.data$Zexit, type = "lead")) %>%
                  # prevent crossings to be counted across hits
                  # shift ReturnID to find out if next crossing belongs to the same ReturnID
                  dplyr::filter(.data$ReturnID == data.table::shift(.data$ReturnID, type = "lead", fill = -1L)) %>%
                  # + 1) - 1L is to counteract symmetry of as.integer at 0: as.integer(-0.1) -> 0, but needs to be -1
                  mutate(Xvoxel = as.integer(pmin(.data$Xentering, .data$Xexit) + 1) - 1L,
                         Yvoxel = as.integer(pmin(.data$Yentering, .data$Yexit) + 1) - 1L,
                         Zvoxel = as.integer(pmin(.data$Zentering, .data$Zexit) + 1) - 1L) %>%
                  # consider only crossings within the block
                  filter(.data$Xvoxel >= minX_scaled & .data$Xvoxel < maxX_scaled,
                         .data$Yvoxel >= minY_scaled & .data$Yvoxel < maxY_scaled,
                         .data$Zvoxel >= minZ_scaled & .data$Zvoxel < maxZ_scaled) %>%
                  # calculate PL, and scale to pc-coordinate system units
                  mutate(PL = sqrt(((.data$Xexit - .data$Xentering) * res["x"]) ^ 2 + ((.data$Yexit - .data$Yentering) * res["y"]) ^ 2 + ((.data$Zexit - .data$Zentering) * res["z"]) ^ 2)) %>%
                  # prevent case when pulse goes through grid crossings to produce 2 crossings
                  filter(.data$PL != 0) %>%
                  select(-.data$Xexit, -.data$Yexit, -.data$Zexit, -.data$Xorigin, -.data$Yorigin, -.data$Zorigin)


                #### voxel mode LAD ####

                if (voxel_mode == "LAD") {
                  hit_voxels <- crossings %>%
                    # no targets outside of the voi should be stored as hit voxels
                    filter(.data$XYZisHit == 1,
                           .data$X >= minX_scaled & .data$X < maxX_scaled,
                           .data$Y >= minY_scaled & .data$Y < maxY_scaled,
                           .data$Z >= minZ_scaled & .data$Z < maxZ_scaled,
                           # only consider crossings "behind"/after the hit in terms of the vector from origin to hit
                           .data$D < 0) %>%
                    group_by(.data$ReturnID) %>%
                    # crossing closest to hit has largest (smallest absolute) D = closest to 0
                    slice(which.max(.data$D)) %>%
                    ungroup() %>%
                    # calculate FPL, and scale to pc-coordinate system units
                    mutate(FPL = sqrt(((.data$X - .data$Xentering) * res["x"]) ^ 2 + ((.data$Y - .data$Yentering) * res["y"]) ^ 2 + ((.data$Z - .data$Zentering) * res["z"]) ^ 2),
                           IsHit = TRUE) %>%
                    select(.data$ReturnID, .data$IsHit, .data$Xvoxel, .data$Yvoxel, .data$Zvoxel, .data$PL, .data$FPL)

                  # between each row of crossings and the next, there was a voxel crossed
                  empty_voxels <- crossings %>%
                    # only consider crossings between hit and origin in terms of the vector from origin to hit
                    filter(.data$D > 0) %>%
                    mutate(FPL = 0,
                           IsHit = FALSE) %>%
                    select(.data$Xvoxel, .data$Yvoxel, .data$Zvoxel, .data$ReturnID, .data$IsHit, .data$PL, .data$FPL)


                  #### voxel mode OCC ####
                } else if (voxel_mode == "OCC") {
                  # simpler version and faster to calculate than for LAD
                  hit_voxels <- pc_scaled_tile %>%
                    filter(.data$XYZisHit == 1, !.data$IsOccluded) %>%
                    mutate(Xvoxel = as.integer(floor(.data$X)),
                           Yvoxel = as.integer(floor(.data$Y)),
                           Zvoxel = as.integer(floor(.data$Z)),
                           IsHit = TRUE) %>%
                    filter(.data$Xvoxel >= minX_scaled & .data$Xvoxel < maxX_scaled,
                           .data$Yvoxel >= minY_scaled & .data$Yvoxel < maxY_scaled,
                           .data$Zvoxel >= minZ_scaled & .data$Zvoxel < maxZ_scaled) %>%
                    select(.data$ReturnID, .data$IsHit, .data$IsOccluded, .data$Xvoxel, .data$Yvoxel, .data$Zvoxel)

                  # between each row of crossings and the next, there was a voxel crossed
                  empty_voxels <- crossings %>%
                    # only consider crossings between hit and origin in terms of the vector from origin to hit
                    filter(.data$D > 0) %>%
                    mutate(FPL = 0,
                           IsHit = FALSE) %>%
                    select(.data$Xvoxel, .data$Yvoxel, .data$Zvoxel, .data$ReturnID, .data$IsHit, .data$IsOccluded)
                }


                #### summarize per voxel ####

                if (voxel_mode == "LAD") {
                  vox_res <- hit_voxels %>%
                    bind_rows(empty_voxels) %>%
                    mutate(EPL = -log(1 - .data$PL * ac_single) / ac_single,
                           EFPL = -log(1 - .data$FPL * ac_single) / ac_single) %>%
                    group_by(.data$Xvoxel, .data$Yvoxel, .data$Zvoxel) %>%
                    summarise(Voxel_N = n(),
                              Hits = sum(.data$IsHit),

                              # path lengths
                              MPL = mean(.data$PL, na.rm = TRUE),
                              MEPL =  mean(.data$EPL, na.rm = TRUE),

                              # free path lengths
                              MFPL = mean(.data$FPL, na.rm = TRUE),
                              MEFPL = mean(.data$EFPL, na.rm = TRUE),
                              MEFPLInd = mean(.data$EFPL * .data$IsHit, na.rm = TRUE),

                              # ratio between empirical variance to mean of the effective path length
                              ae = .data$var(.data$EPL, na.rm = TRUE) / .data$MEPL) %>%
                    ungroup() %>%
                    mutate(# N relevant for LAD computations (only empty + hit counts)
                      # Voxel_N = sum of interactions within voxel

                      # relative density index
                      RDI = .data$Hits / .data$Voxel_N,
                      # contact frequency
                      CF = .data$RDI / .data$MPL,
                      # modified contact frequency
                      MCF = .data$RDI / .data$MFPL,
                      # theoretical bias-corrected Maximum-Likelihood estimator
                      TBC_MLE = .data$RDI / .data$MEFPL - .data$MEFPLInd / (.data$Voxel_N * .data$MEFPL ^ 2),
                      # Beer-Lambert
                      BL = -log(1 - .data$RDI) / .data$MPL,
                      # theoretical bias-corrected Beer-Lambert, equal path lengths
                      TBC_BL1 = ifelse(.data$RDI < 1,
                                       -1 / .data$MEPL * (log(1 - .data$RDI) + .data$RDI / (2 * .data$Voxel_N * (1 - .data$RDI))),
                                       log(2 * .data$Voxel_N + 2) / .data$MEPL),
                      # theoretical bias-corrected Beer-Lambert, unequal path lengths
                      TBC_BL2 = 1 / .data$ae * (1 - sqrt(1 - 2 * .data$ae * .data$TBC_BL1)))

                } else if (voxel_mode == "OCC") {
                  vox_res <- hit_voxels %>%
                    bind_rows(empty_voxels) %>%
                    group_by(.data$Xvoxel, .data$Yvoxel, .data$Zvoxel) %>%
                    summarise(Voxel_N = n(),
                              Hits = sum(.data$IsHit),
                              Empty = sum(!.data$IsHit & !.data$IsOccluded),
                              Occluded =  sum(.data$IsOccluded)) %>%
                    ungroup()
                }

                vox_res
              }
            })

            # output format Vox
            vox <- methods::new("Vox")

            # fix problem with pbmclapply when only 1 tile is processed
            if (nrow(tiles) == 1) {
              vox_df <- vox_df[[1]][[1]]
            }

            vox@data <- vox_df %>%
              bind_rows()

            vox@extent <- voi_global
            vox@resolution <- res
            vox@crs <- st_crs(rays)
            vox@mode <- voxel_mode
            vox@height_normalized <- FALSE

            vox
          })


