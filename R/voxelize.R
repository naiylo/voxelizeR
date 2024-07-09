utils::globalVariables(c(
  "EFPL", "EPL", "FPL", "Hits", "IsHit", "IsOccluded", "MEFPL", "MEFPLInd",
  "MEPL", "MFPL", "MPL", "PL", "RDI", "ReturnID", "TBC_BL1", "Voxel_N",
  "Xentering", "Xexit", "Xold", "Xvoxel", "Yentering", "Yexit", "Yold",
  "Yvoxel", "Zentering", "Zexit", "Zold", "Zvoxel", "ae", "d", "var"
))

#' Voxelize point cloud with ray-tracing
#'
#' @importFrom methods new
#' @importFrom sf st_coordinates st_geometry_type st_as_binary st_as_sf st_intersection st_distance st_intersects st_as_sfc st_crs st_bbox
#' @importFrom methods setGeneric setMethod
#' @importFrom parallel mclapply
#' @importFrom pbmcapply pbmclapply
#' @importFrom dplyr arrange bind_rows group_by mutate select slice summarise ungroup filter
#' @importFrom data.table data.table
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
#' @examples
#' # Load required packages
#' library(lidR)
#' library(pracma)
#' library(dplyr)
#' library(data.table)
#' library(sf)
#'
#' # Read in LAS data
#' data_file <- system.file("extdata", "H7_LS_F2_H20_200901-120129.laz", package = "voxelizer")
#' las <- readLAS(data_file)
#'
#' # Ensure the CRS of the LAS object
#' epsg(las) <- 32631
#'
#' # Create a trajectory for the LAS object
#' traj_file <- system.file("extdata", "H7_LS_F2_H20_200901-120129.traj", package = "voxelizer")
#' colnames <- c('gpstime', 'roll', 'pitch', 'yaw', 'Xorigin', 'Yorigin', 'Zorigin')
#' traj <- fread(traj_file, col.names = colnames) %>%
#'   select(gpstime, Xorigin, Yorigin, Zorigin) %>%
#'   rename(Xtraj = Xorigin,
#'          Ytraj = Yorigin,
#'          Ztraj = Zorigin) %>%
#'   mutate(gpstime = as.numeric(gpstime),
#'          Xtraj = as.numeric(Xtraj),
#'          Ytraj = as.numeric(Ytraj),
#'          Ztraj = as.numeric(Ztraj))
#'
#' # Create a subset of the LAS data
#' laz <- las[1:10,]
#'
#' # Compute rays from LAS points and trajectory
#' rays <- las2rays(laz, traj)
#'
#' # Prepare tiles
#' tiles <- prepare_tiles(c(xmin = 682130, ymin = 5763580, xmax = 682300, ymax = 5763680),
#'                        res = c(x = 1, y = 1),
#'                        tilesize = c(20, 20),
#'                        crs = 32631)
#'
#' # Prepare aoi
#' aoi <- st_bbox(tiles) %>%
#'   as.numeric() %>%
#'   setNames(names(st_bbox(tiles)))
#'
#' # Prepare zrange
#' zrange <- c(50, 55) %>%
#'   setNames(c("zmin", "zmax"))
#'
#' # Prepare res
#' res <- c(x = 1, y = 1, z = 1)
#'
#' # Use voxelize function
#' vox <- suppressWarnings(voxelize(
#'   rays = rays,
#'   tiles = tiles,
#'   zrange = zrange,
#'   res = res,
#'   ac_single = 0.001,
#'   voxel_mode = "OCC",
#'   process_tiles_parallel = 1,
#'   process_order_tiles = "random"
#' ))
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
              select(X, Y, Z, Xorigin, Yorigin, Zorigin, XYZisHit, ReturnID)

            ## occlusion processing: extend pulses to zrange["zmin"] - res["z"] (subtract res to avoid edge effects)
            if (voxel_mode == "OCC") {
              pc_occlusion <- data.table::copy(rays@data) %>%
                filter(ReturnNumber == NumberOfReturns) %>%
                select(X, Y, Z, Xorigin, Yorigin, Zorigin, XYZisHit, ReturnID) %>%
                mutate(lx = X - Xorigin,
                       ly = Y - Yorigin,
                       lz = Z - Zorigin,
                       d = (zrange["zmin"] - res["z"] - Zorigin) / lz,
                       Xold = X,
                       Yold = Y,
                       Zold = Z,
                       X = d * lx + Xorigin,
                       Y = d * ly + Yorigin,
                       Z = zrange["zmin"] - res["z"],
                       Xorigin = Xold,
                       Yorigin = Yold,
                       Zorigin = Zold) %>%
                select(-lx, -ly, -lz, -d, -Xold, -Yold, -Zold) %>%
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
              mutate(X = (X - voi_global["xmin"]) / res["x"],
                     Y = (Y - voi_global["ymin"]) / res["y"],
                     Z = (Z - voi_global["zmin"]) / res["z"],
                     Xorigin = (Xorigin - voi_global["xmin"]) / res["x"],
                     Yorigin = (Yorigin - voi_global["ymin"]) / res["y"],
                     Zorigin = (Zorigin - voi_global["zmin"]) / res["z"])

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
                                        filter(!is.infinite(abs(D)))
                                    }) %>%
                  bind_rows() %>%
                  arrange(ReturnID, D) %>%
                  # the pulse entering is where it exits from the previous crossing
                  mutate(Xexit = D * (Xorigin - X) + X,
                         Yexit = D * (Yorigin - Y) + Y,
                         Zexit = D * (Zorigin - Z) + Z,
                         Xentering = data.table::shift(Xexit, type = "lead"),
                         Yentering = data.table::shift(Yexit, type = "lead"),
                         Zentering = data.table::shift(Zexit, type = "lead")) %>%
                  # prevent crossings to be counted across hits
                  # shift ReturnID to find out if next crossing belongs to the same ReturnID
                  dplyr::filter(ReturnID == data.table::shift(ReturnID, type = "lead", fill = -1L)) %>%
                  # + 1) - 1L is to counteract symmetry of as.integer at 0: as.integer(-0.1) -> 0, but needs to be -1
                  mutate(Xvoxel = as.integer(pmin(Xentering, Xexit) + 1) - 1L,
                         Yvoxel = as.integer(pmin(Yentering, Yexit) + 1) - 1L,
                         Zvoxel = as.integer(pmin(Zentering, Zexit) + 1) - 1L) %>%
                  # consider only crossings within the block
                  filter(Xvoxel >= minX_scaled & Xvoxel < maxX_scaled,
                         Yvoxel >= minY_scaled & Yvoxel < maxY_scaled,
                         Zvoxel >= minZ_scaled & Zvoxel < maxZ_scaled) %>%
                  # calculate PL, and scale to pc-coordinate system units
                  mutate(PL = sqrt(((Xexit - Xentering) * res["x"]) ^ 2 + ((Yexit - Yentering) * res["y"]) ^ 2 + ((Zexit - Zentering) * res["z"]) ^ 2)) %>%
                  # prevent case when pulse goes through grid crossings to produce 2 crossings
                  filter(PL != 0) %>%
                  select(-Xexit, -Yexit, -Zexit, -Xorigin, -Yorigin, -Zorigin)


                #### voxel mode LAD ####

                if (voxel_mode == "LAD") {
                  hit_voxels <- crossings %>%
                    # no targets outside of the voi should be stored as hit voxels
                    filter(XYZisHit == 1,
                           X >= minX_scaled & X < maxX_scaled,
                           Y >= minY_scaled & Y < maxY_scaled,
                           Z >= minZ_scaled & Z < maxZ_scaled,
                           # only consider crossings "behind"/after the hit in terms of the vector from origin to hit
                           D < 0) %>%
                    group_by(ReturnID) %>%
                    # crossing closest to hit has largest (smallest absolute) D = closest to 0
                    slice(which.max(D)) %>%
                    ungroup() %>%
                    # calculate FPL, and scale to pc-coordinate system units
                    mutate(FPL = sqrt(((X - Xentering) * res["x"]) ^ 2 + ((Y - Yentering) * res["y"]) ^ 2 + ((Z - Zentering) * res["z"]) ^ 2),
                           IsHit = TRUE) %>%
                    select(ReturnID, IsHit, Xvoxel, Yvoxel, Zvoxel, PL, FPL)

                  # between each row of crossings and the next, there was a voxel crossed
                  empty_voxels <- crossings %>%
                    # only consider crossings between hit and origin in terms of the vector from origin to hit
                    filter(D > 0) %>%
                    mutate(FPL = 0,
                           IsHit = FALSE) %>%
                    select(Xvoxel, Yvoxel, Zvoxel, ReturnID, IsHit, PL, FPL)


                  #### voxel mode OCC ####
                } else if (voxel_mode == "OCC") {
                  # simpler version and faster to calculate than for LAD
                  hit_voxels <- pc_scaled_tile %>%
                    filter(XYZisHit == 1, !IsOccluded) %>%
                    mutate(Xvoxel = as.integer(floor(X)),
                           Yvoxel = as.integer(floor(Y)),
                           Zvoxel = as.integer(floor(Z)),
                           IsHit = TRUE) %>%
                    filter(Xvoxel >= minX_scaled & Xvoxel < maxX_scaled,
                           Yvoxel >= minY_scaled & Yvoxel < maxY_scaled,
                           Zvoxel >= minZ_scaled & Zvoxel < maxZ_scaled) %>%
                    select(ReturnID, IsHit, IsOccluded, Xvoxel, Yvoxel, Zvoxel)

                  # between each row of crossings and the next, there was a voxel crossed
                  empty_voxels <- crossings %>%
                    # only consider crossings between hit and origin in terms of the vector from origin to hit
                    filter(D > 0) %>%
                    mutate(FPL = 0,
                           IsHit = FALSE) %>%
                    select(Xvoxel, Yvoxel, Zvoxel, ReturnID, IsHit, IsOccluded)
                }


                #### summarize per voxel ####

                if (voxel_mode == "LAD") {
                  vox_res <- hit_voxels %>%
                    bind_rows(empty_voxels) %>%
                    mutate(EPL = -log(1 - PL * ac_single) / ac_single,
                           EFPL = -log(1 - FPL * ac_single) / ac_single) %>%
                    group_by(Xvoxel, Yvoxel, Zvoxel) %>%
                    summarise(Voxel_N = n(),
                              Hits = sum(IsHit),

                              # path lengths
                              MPL = mean(PL, na.rm = TRUE),
                              MEPL =  mean(EPL, na.rm = TRUE),

                              # free path lengths
                              MFPL = mean(FPL, na.rm = TRUE),
                              MEFPL = mean(EFPL, na.rm = TRUE),
                              MEFPLInd = mean(EFPL * IsHit, na.rm = TRUE),

                              # ratio between empirical variance to mean of the effective path length
                              ae = var(EPL, na.rm = TRUE) / MEPL) %>%
                    ungroup() %>%
                    mutate(# N relevant for LAD computations (only empty + hit counts)
                      # Voxel_N = sum of interactions within voxel

                      # relative density index
                      RDI = Hits / Voxel_N,
                      # contact frequency
                      CF = RDI / MPL,
                      # modified contact frequency
                      MCF = RDI / MFPL,
                      # theoretical bias-corrected Maximum-Likelihood estimator
                      TBC_MLE = RDI / MEFPL - MEFPLInd / (Voxel_N * MEFPL ^ 2),
                      # Beer-Lambert
                      BL = -log(1 - RDI) / MPL,
                      # theoretical bias-corrected Beer-Lambert, equal path lengths
                      TBC_BL1 = ifelse(RDI < 1,
                                       -1 / MEPL * (log(1 - RDI) + RDI / (2 * Voxel_N * (1 - RDI))),
                                       log(2 * Voxel_N + 2) / MEPL),
                      # theoretical bias-corrected Beer-Lambert, unequal path lengths
                      TBC_BL2 = 1 / ae * (1 - sqrt(1 - 2 * ae * TBC_BL1)))

                } else if (voxel_mode == "OCC") {
                  vox_res <- hit_voxels %>%
                    bind_rows(empty_voxels) %>%
                    group_by(Xvoxel, Yvoxel, Zvoxel) %>%
                    summarise(Voxel_N = n(),
                              Hits = sum(IsHit),
                              Empty = sum(!IsHit & !IsOccluded),
                              Occluded =  sum(IsOccluded)) %>%
                    ungroup()
                }

                vox_res
              }
            })

            # output format Vox
            vox <- new("Vox")

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

            return(vox)
          })


