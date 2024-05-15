#' Permute order of vector randomly
#'
#' @author Benjamin Brede
#'
#' @description
#' This function takes a vector 'x' and returns a new vector with its elements
#' randomly permuted. It uses the 'sample.int' function to generate a random
#' permutation of the vector indices and then reorders the vector accordingly.
#'
#' @param x A vector to be randomly permuted.
#'
#' @return A new vector with the elements of 'x' randomly permuted.
#'
#' @export

permute <- function(x) {

  x[sample.int(length(x))]

}
