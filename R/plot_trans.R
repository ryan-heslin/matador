#' Plot a Matrix Before and After a Linear Transformation
#'
#' @param m A 2 x m matrix
#' @param trans A 2 x 2 matrix representing the transformation to apply to m
#' @param before Color to use plotting the vectors of m. May be hexadecimal.
#' @param after Color to use when potting the vectors of the transformed matrix. May be hexadecimal.
#'
#' @return A side-by-side plot, the first panel depicting the original matrix, the second the matrix after its transformation.
#' @export
#'
#' @examples #45-degree rotation counterclockwise
#' plot_transform(m = matrix(c(3, 1, 1, 2),
#'trans = matrix(c(1/sqrt(2), 1/sqrt(2), -1/sqrt(2), 1/sqrt(2))))
plot_transform <- function(m, trans, before = "blue", after = "red"){

  image <- trans %*% m

  p1 <- matador::plot_trans(m, color = before)
  p2 <- matador::plot_mat(image, color = after)

  gridExtra::grid.arrange(p1, p2, ncol = 2)
}
