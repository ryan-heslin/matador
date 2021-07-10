#' Create a Square Matrix
#' @description This simple convenience function creates a square matrix from
#' elements passed via  `...`. It will throw an error if the number of
#' elements passed to `...` is not a perfect square.
#' @param ... Elements of the matrix as vectors whose total number of elements is
#' a perfect square. Elements of differing types are automatically coerced using
#' `c`.
#' @param byrow Argument passed along to `matrix`. Defaults to FALSE.
#' @return An n x n matrix, where `n` is equal to the length of `...` after
#' concatenation.
#' @export
#'
#' @examples
#' square(1, 5, 6, 7, 8, 9, 1, 2, -1)
#' # Also works with vectors of differing length
#' square(-10:0, 5, 2, 3, 0, 9)
square <- function(..., byrow = FALSE) {
  dots <- c(...)
  len <- sqrt(length(dots))
  if (len %% 1 != 0) {
    stop("Cannot construct square matrix from ", length(dots), " elements")
  }
  matrix(dots, nrow = len, byrow = byrow)
}
