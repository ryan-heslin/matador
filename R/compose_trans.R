#' Title
#'
#' @param mats A list of matrices that can be multiplied in the conventional order, that is A(B(C)). The range of each matrix must equal the domain of the matrix to its right,
#' per the conventional definition of matrix multiplication. Vectors may also be used if represented as n x 1 matrices, assuming they obey the above constraint.
#'
#' @return The composition of the transformations (i.e., the product of multiplying the matrices in order)
#' @export
#'
#' @examples a <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
#' b <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' compose(list(b,a))
compose_trans <- function(mats) {
  purrr::reduce(mats, `*`)
}
