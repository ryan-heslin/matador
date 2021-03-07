#' Compose a Series of Matrix Transformations
#'@description This simple function multiplies out a list of matrices, yielding the composition of the transformations.
#' @param mats A list of matrices that can be multiplied in the conventional order, i.e., A(B(C)). The range of each matrix must equal the domain of the matrix to its right,
#' per the conventional definition of matrix multiplication. Vectors may also be included if represented as n x 1 matrices, assuming they obey the above constraint.
#'
#' @return The composition of the transformations (i.e., the product of multiplying the matrices in order)
#' @export
#'
#' @examples a <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3)
#' b <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
#' compose(list(b,a))
#' #Matrix and inverse gives the identity
#' c <- matrix(c(5, 9, 7, 2), nrow = 2)
#' compose_trans(c, solve(c))
compose_trans <- function(mats) {
  if(!all(purrr::map_chr(mats, mode) == "numeric")){
    stop("Cannot compute product of non-numeric matrices.")
  }
  purrr::reduce(mats, `%*%`)
}
