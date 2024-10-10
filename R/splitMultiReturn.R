#' Split multi-return into single return point cloud
#'
#' @importFrom utils head
#' @importFrom stats setNames
#' @importFrom dplyr arrange filter group_by mutate ungroup
#'
#' @author Benjamin Brede
#'
#' @description
#' This function processes a point cloud with multi-return laser pulses, splitting each multi-return
#' into individual single-return paths. It ensures that each pulse ends at the first return and
#' initiates a new path from the first to the subsequent returns.
#'
#' @seealso
#' \link{las2rays}
#'
#' @name splitMultiReturn
#' @aliases splitMultiReturn,Rays-method
#' @rdname splitMultiReturn-methods
#' @docType methods
#' @usage splitMultiReturn(rays, ...)
#'
#' @param rays An object of class Rays, which contains the point cloud data with multi-return pulses.
#' @param ... Additional arguments passed to other methods or functions.
#'
#' @return An object of class Rays with multi-return pulses split into single-return paths.
#'
#' @examples
#' # Load necessary libraries
#' library(voxelizer)
#' library(data.table)
#' library(dplyr)
#' library(lidR)
#'
#' # Create an S4 LAS object
#' data_file <- system.file("extdata", "H7_LS_F2_H20_200901-120129.laz", package = "voxelizer")
#' las <- readLAS(data_file)
#' laz <- las[1:100]  # Use a subset for the example
#'
#' # Create trajectory for the object
#' traj_file <- system.file("extdata", "H7_LS_F2_H20_200901-120129.traj", package = "voxelizer")
#' traj <- fread(traj_file, col.names = c('gpstime', 'roll', 'pitch',
#'                                        'yaw', 'Xorigin', 'Yorigin',
#'                                        'Zorigin')) %>%
#'   select(gpstime, Xorigin, Yorigin, Zorigin) %>%
#'   rename(Xtraj = Xorigin, Ytraj = Yorigin, Ztraj = Zorigin)
#'
#' # Create an S4 Rays object
#' rays <- las2rays(laz, traj)
#'
#' # Apply the splitMultiReturn function
#' modified_rays <- splitMultiReturn(rays)
#'
#' # Check the result
#' head(modified_rays@data)
#'
#'
#' @export

setGeneric("splitMultiReturn",
           function(rays, ...) standardGeneric("splitMultiReturn"))

#' @rdname splitMultiReturn-methods

setMethod("splitMultiReturn",
          signature = c("Rays"),
          definition = function(rays) {

            # Validate input
            if (!inherits(rays, "Rays")) {
              stop("Error: Input must be of class 'Rays'.")
            }

            if (!"data" %in% slotNames(rays)) {
              stop("Error: The input object does not contain the required 'data' slot.")
            }

            if (!all(c('Xtraj', 'Ytraj', 'Ztraj', 'NumberOfReturns', 'ReturnNumber', 'gpstime') %in% names(rays@data))) {
              stop("Error: The 'data' slot must contain the necessary columns: 'Xtraj', 'Ytraj', 'Ztraj', 'NumberOfReturns', 'ReturnNumber', 'gpstime'.")
            }

            # Check if pulseID exists; if not, add it
            if (!'pulseID' %in% names(rays@data)) {
              rays <- addPulseID(rays)
            }

            # Extract the point cloud data
            pc <- rays@data %>%
              dplyr::mutate(Xorigin = .data$Xtraj,
                            Yorigin = .data$Ytraj,
                            Zorigin = .data$Ztraj)

            # Handle single returns (no changes needed)
            single_return <- pc %>%
              dplyr::filter(.data$NumberOfReturns == 1)

            # Handle multi-return cases
            multi_return <- pc %>%
              dplyr::filter(.data$NumberOfReturns > 1) %>%
              dplyr::group_by(.data$pulseID) %>%
              dplyr::mutate(Xorigin = c(.data$Xorigin[1], utils::head(.data$X, -1)),
                            Yorigin = c(.data$Yorigin[1], utils::head(.data$Y, -1)),
                            Zorigin = c(.data$Zorigin[1], utils::head(.data$Z, -1))) %>%
              dplyr::ungroup()

            # Combine single and multi-return points and sort them by time and return number
            rays@data <- rbind(single_return, multi_return) %>%
              dplyr::arrange(.data$gpstime, .data$ReturnNumber) %>%
              stats::setNames(names(rays@data))

            rays
          })
