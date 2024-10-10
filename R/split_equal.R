#' Split vector into equal parts
#'
#' @author Benjamin Brede
#'
#' @description
#' This function divides a vector x into n groups of equal length, or the vector in
#' groups of length l If n is 1, it returns the original vector. Otherwise, it uses
#' the cut function to create a factor that splits the sequence along the vector into
#' n groups, which is then used by the split function to divide the vector accordingly.
#'
#' @param x vector to split
#' @param n number of groups to split x into
#'
#' @return list with parts of the vector
#'
#' @examples
#' # Example 1: Split vector into 3 groups of roughly equal length
#' split_equal(1:10, 3)
#'
#' # Example 2: Split vector into 5 groups of roughly equal length
#' split_equal(1:10, 5)
#'
#' # Example 3: Attempt to split vector into 20 groups (some will be empty)
#' split_equal(1:10, 20)
#'
#' # Example 4: Split an empty vector into 3 groups (returns a list of 3 empty vectors)
#' split_equal(integer(0), 3)
#'
#' @export

split_equal <- function(x, n) {
  # Input validation
  if (!is.vector(x)) {
    stop("Error: 'x' must be a vector.")
  }

  if (!is.numeric(n) || n <= 0 || n %% 1 != 0) {
    stop("Error: 'n' must be a positive integer.")
  }

  if (n == 1) {
    return(list(x))
  }

  if (length(x) == 0) {
    return(rep(list(integer(0)), n))
  }

  groups <- cut(seq_along(x), n, labels = FALSE)

  # Ensure that the groups are as evenly distributed as possible
  split(x, groups)
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
#' @examples
#' # Example 1: Split vector into groups of length 3
#' split_equal2(1:10, 3)
#'
#' # Example 2: Split vector into groups of length 5
#' split_equal2(1:10, 5)
#'
#' # Example 3: Split vector into groups of length greater than the vector length
#' split_equal2(1:10, 20)
#'
#' # Example 4: Split an empty vector into groups of length 3 (returns an empty list)
#' split_equal2(integer(0), 3)
#'
#' @export

split_equal2 <- function(x, l) {
  # Input validation
  if (!is.vector(x)) {
    stop("Error: 'x' must be a vector.")
  }

  if (!is.numeric(l) || l <= 0 || l %% 1 != 0) {
    stop("Error: 'l' must be a positive integer.")
  }

  if (length(x) == 0) {
    return(list())
  }

  # Split the vector into groups of length 'l'
  split(x, ceiling(seq_along(x) / l))
}

