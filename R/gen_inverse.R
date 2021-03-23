#' Solve Systems Using the Generalized Inverse
#' @description This function uses the pseudoinverse method to solve systems of
#' equations given in non-square matrices.
#' @param A An n x m matrix where n>m
#' @param b An n x 1 vector for which to solve Ax=b
#'
#' @return The m x 1 vector x that solve the system, if it exists; otherwise, an
#' error
#' @export
#'
#' @examples
#' gen_inverse(matrix(1:6, nrow = 3), 1:3)
gen_inverse <- function(A, b){

  solve(t(A) %*% A) %*% t(A) %*% b
}
