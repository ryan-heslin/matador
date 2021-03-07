#' Plot a Matrix Before and After a Linear Transformation
#'
#' @param m A matrix of any dimension for which multiplication by trans (or, if trans is a list, its composition) is defined.
#' By default, the 2 x 2 identity.
#' @param trans A 2 x m matrix representing the transformation to apply to m, for which multiplicaiton by m is defined.. Alternately, a list of matrices for which multiplication is defined in the conventional
#' order. If such a list is passed, the leftmost matrix (the final transformation) must have two dimensions.
#' @param before Color to use plotting the vectors of m. May be hexadecimal.
#' @param after Color to use when potting the vectors of the transformed matrix. May be hexadecimal.
#'
#' @return A side-by-side plot, the first panel depicting the original matrix, the second the matrix after its transformation. If m is not two-dimensional, only
#' a plot of the image.
#' @export
#'
#' @examples #45-degree rotation counterclockwise
#' plot_transform(m = matrix(c(3, 1, 1, 2),
#'trans = matrix(c(1/sqrt(2), 1/sqrt(2), -1/sqrt(2), 1/sqrt(2))))
plot_transform <- function(m = diag(nrow =2) , trans, before = "blue", after = "red"){

  # Compose transformation if need be
  if(is.list(trans)){
    trans <- compose_trans(trans)
  }
  image <- trans %*% m

  if(nrow(m)!= 2){
    message("Cannot plot input matrix of dimension ", nrow(m))
    return(plot_mat(image, color = after))
  }
  p1 <- matador::plot_mat(m, color = before)
  p2 <- matador::plot_mat(image, color = after)

  gridExtra::grid.arrange(p1, p2, ncol = 2, left = "Before", right = "After")
}
