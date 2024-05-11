#' Split vector into equal parts
#'
#' This function divides a vector 'x' into 'n' groups of equal length, or the vector in
#' groups of length 'l' If 'n' is 1, it returns the original vector. Otherwise, it uses
#' the 'cut' function to create a factor that splits the sequence along the vector into
#' 'n' groups, which is then used by the 'split' function to divide the vector accordingly.
#'
#' @param x vector to split
#' @param n number of groups to split x into
#' @param l length of groups
#'
#' @return list with parts of the vector
#'
#' @export
#'
#' @examples
#' split_equal(1:10, 2) # returns list(1:5, 6:10)
#' split_equal(1:10, 5) # returns list(1:2, 3:4, 5:6, 7:8, 9:10)
#' split_equal(1:10, 1) # returns list(1:10)
#'
#' split_equal_2(1:10, l = 2) # returns list(1:5, 6:10)
#' split_equal_2(1:10, l = 3) # returns list(1:3, 4:6, 7:9, 10)
#' split_equal_2(1:10, l = 5) # returns list(1:2, 3:4, 5:6, 7:8, 9:10)

split_equal <- function(x, n) {

  if (n == 1) {
    list(x)
  } else {
    split(x, cut(seq_along(x), n, labels = FALSE))
  }

}

split_equal2 <- function(x, l) {

  split(x, ceiling(seq_along(x) / l))

}

