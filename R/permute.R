# SPDX-FileCopyrightText: 2024 Helmholtz Centre Potsdam - GFZ German Research Centre for Geosciences
# SPDX-FileCopyrightText: 2024 Benjamin Brede
# SPDX-License-Identifier: GPL-3.0-or-later

#' Permute order of vector randomly
#'
#' @author Benjamin Brede
#'
#' @description
#' This function takes a vector x and returns a new vector with its elements
#' randomly permuted. It uses the sample.int function to generate a random
#' permutation of the vector indices and then reorders the vector accordingly.
#'
#' @param x A vector to be randomly permuted.
#'
#' @return A new vector with the elements of x randomly permuted.
#'
#' @examples
#'
#' # Example 1: Permuting a simple numeric vector
#' x <- 1:10
#' permuted_x <- permute(x)
#' print(permuted_x)
#'
#' # Example 2: Permuting a character vector
#' y <- c("apple", "banana", "cherry", "date")
#' permuted_y <- permute(y)
#' print(permuted_y)
#'
#' # Example 3: Permuting an empty vector
#' z <- numeric(0)
#' permuted_z <- permute(z)
#' print(permuted_z)
#'
#' # Example 4: Permuting a single-element vector
#' w <- c(42)
#' permuted_w <- permute(w)
#' print(permuted_w)
#'
#' # Example 5: Permuting a vector with repeated elements
#' v <- c(1, 1, 2, 2, 3, 3)
#' permuted_v <- permute(v)
#' print(permuted_v)
#'
#' @export

permute <- function(x) {

  # Check if input is a valid vector
  if (!is.vector(x)) {
    stop("Error: Input 'x' must be a vector.")
  }

  # If the vector has no elements, return the empty vector
  if (length(x) == 0) {
    warning("Input vector is empty. Returning the same empty vector.")
    return(x)
  }

  # If the vector has only one element, return it as is
  if (length(x) == 1) {
    warning("Input vector has only one element. Returning the same vector.")
    return(x)
  }

  # Permute the vector using sample.int
  tryCatch({
    return(x[sample.int(length(x))])
  }, error = function(e) {
    stop("Error in permuting the vector: ", e$message)
  })
}
