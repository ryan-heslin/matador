#' Compose a Series of Matrix Transformations
#'@description This function multiplies out a list of matrices and/or vectors, yielding the composition of the transformations.
#' @param mats A list of matrices and/or vectors that can be multiplied in the conventional order, i.e., A(B(C)). Each matrix must have as many columns as the matrix
#' to its right has rows, per the conventional definition of matrix multiplication.
#' Vectors may also be included in the sequence, assuming they obey the above constraint.
#'
#' @return The composition of the transformations (i.e., the product of multiplying the matrices in order)
#' @export
#'
#' @examples a <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
#' b <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' compose_trans(list(b,a))
#'
#' #Matrix and inverse gives the identity
#' c <- matrix(c(5, 9, 7, 2), nrow = 2)
#' compose_trans(list(c, solve(c)))
compose_trans <- function(mats) {
  if(!all(purrr::map_chr(mats, mode) == "numeric")){
    stop("Cannot compute product of non-numeric matrices.")
  }
  purrr::reduce(mats, `%*%`)
}
