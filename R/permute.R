#' Permute order of vector randomly
#'
#' This function takes a vector 'x' and returns a new vector with its elements
#' randomly permuted. It uses the 'sample.int' function to generate a random
#' permutation of the vector indices and then reorders the vector accordingly.
#'
#' @param x A vector to be randomly permuted.
#' @return A new vector with the elements of 'x' randomly permuted.
#'
#' @export
#'
#' @examples
#' permute(c(1, 2, 3, 4)) # could return c(2, 4, 1, 3)
#' permute(letters[1:5]) # could return c('e', 'a', 'd', 'b', 'c')

permute <- function(x) {

  x[sample.int(length(x))]

}
