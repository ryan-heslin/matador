#' Plot a Matrix Before and After a Linear Transformation
#' @description This function plots the vectors within a matrix, multiplies the matrix by the provided transformation,
#' then plots the result. It makes it easy to visualize even complex linear transformations on the two-dimensional plane.
#' @param m A matrix of any dimension for which multiplication by trans (or, if trans is a list, its composition) is defined.
#' By default, the 2 x 2 identity.
#' @param trans A 2 x m matrix representing the transformation to apply to m, for which multiplication by m is defined.
#'  Alternately, a list of matrices for which multiplication is defined in the conventional
#' order. If such a list is passed, the leftmost matrix (the final transformation) must have two dimensions.
#' @param before Color to use plotting the vectors of m. May be hexadecimal.
#' @param after Color to use when potting the vectors of the transformed matrix. May be hexadecimal.
#' @param fix_coords Passed on tho plot_mat.
#' @return A side-by-side plot, the first panel depicting the original matrix, the second the matrix after its transformation.
#'  If _m_is not two-dimensional, only
#' a plot of the image of the transformation.
#' @export
#'
#' @examples #45-degree rotation counterclockwise
#' plot_transform(m = matrix(c(3, 1, 1, 2), nrow =2),
#' trans = matrix(c(1/sqrt(2), 1/sqrt(2), -1/sqrt(2), 1/sqrt(2)), nrow =2))
#' #Projection of unit cube onto xy plane
#' plot_transform(m = diag(nrow =3), trans= matrix(c(1, 0, 0, 1, .5, .5), nrow =2))
#'
plot_transform <- function(m = diag(nrow =2) , trans, before = "blue", after = "red", fix_coords = FALSE){

  # Compose transformation if need be
  if(is.list(trans)){
    trans <- compose_trans(trans)
  }
  image <- trans %*% m


  if(nrow(m)!= 2){
    message("Cannot plot input matrix of dimension ", nrow(m))
    return(plot_mat(image, color = after, fix_coords = fix_coords)) +ggplot2::ggtitle("T(x)")
  }

  p1 <- matador::plot_mat(m, color = before, fix_coords = fix_coords) + ggplot2::ggtitle("X")
  p2 <- matador::plot_mat(image, color = after, fix_coords = fix_coords)+ggplot2::ggtitle("T(x)")

  cowplot::plot_grid(p1, p2)
}
