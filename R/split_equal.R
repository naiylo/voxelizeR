#' Split vector into equal parts
#'
#' @author Benjamin Brede
#'
#' @description
#' This function divides a vector 'x' into 'n' groups of equal length, or the vector in
#' groups of length 'l' If 'n' is 1, it returns the original vector. Otherwise, it uses
#' the 'cut' function to create a factor that splits the sequence along the vector into
#' 'n' groups, which is then used by the 'split' function to divide the vector accordingly.
#'
#' @param x vector to split
#' @param n number of groups to split x into
#'
#' @return list with parts of the vector
#'
#' @export

split_equal <- function(x, n) {
  if (n == 1) {
    list(x)
  } else if (length(x) == 0) {
    rep(list(integer(0)), n)
  } else {
    groups <- cut(seq_along(x), n, labels = FALSE)
    # Ensure that the groups are as evenly distributed as possible
    split(x, groups)
  }
}

#' Split vector into equal parts
#'
#' @author Benjamin Brede
#'
#' @description
#' This function divides a vector 'x' into groups of length 'l'.
#'
#' @param x vector to split
#' @param l length of groups
#'
#' @return list with parts of the vector
#'
#' @export

split_equal2 <- function(x, l) {

  split(x, ceiling(seq_along(x) / l))

}

