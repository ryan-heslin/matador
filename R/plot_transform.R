#' Plot a Matrix Before and After a Linear Transformation
#' @description This function plots the vectors within a matrix, multiplies the matrix by the provided transformation,
#' then plots the result. It makes it easy to visualize even complex linear transformations on the two-dimensional plane.
#' @param trans A 2 x m matrix representing the transformation to apply to m, for which multiplication by m is defined.
#'  Alternately, a list of matrices for which multiplication is defined in the conventional
#' order. If such a list is passed, the leftmost matrix (the final transformation) must have two dimensions.
#' @param m A matrix of any dimension for which multiplication by trans (or, if trans is a list, its composition) is defined.
#' By default, the 2 x 2 identity.
#' @param cols_before,linetype_before,cols_after,linetype_after
#' Character vectors dictating the `color` and `linetype` aesthetics
#' of `geom_segment`, used to draw each plot. All are passed on to `plot_mat`
#' via its `...` argument and evaluated there. By default, this function uses
#' color to distinguish individual vectors and linetype to distinguish the stage
#' of the transformation, which fits most use cases. Accordingly, if one of
#' `cols_before` and `cols_after` is missing, the other is assigned the
#' value of the one that was supplied; if both are missing, they are assigned
#' the same contrasting colors.
#' @param fix_coords Passed on tho plot_mat. Logical determining whether to
#' force an even aspect ratio on the plots. Defaults to FALSE.
#' @return A side-by-side plot, the first panel depicting the original matrix, the second the matrix after its transformation.
#'  If _m_is not two-dimensional, only
#' a plot of the image of the transformation.
#' @export
#' @seealso \link[matador]{plot_mat}, the function used to render the plots.
#'
#' @examples # 45-degree rotation counterclockwise
#' plot_transform(m = matrix(c(3, 1, 1, 2), nrow =2),
#' trans = matrix(c(1/sqrt(2), 1/sqrt(2), -1/sqrt(2), 1/sqrt(2)), nrow =2))
#' # Projection of unit cube onto xy plane
#' plot_transform(m = diag(nrow =3), trans= matrix(c(1, 0, 0, 1, .5, .5), nrow =2))
#'
plot_transform <-
  function(trans,
           m = diag(nrow = 2),
           cols_before,
           linetype_before = "solid",
           cols_after,
           linetype_after = "dashed",
           fix_coords = FALSE) {
    # Compose transformation if need be
    if (is.list(trans))
      trans <- compose_trans(trans)

    if (missing(cols_before) && !missing(cols_after)) {
      cols_before <- cols_after
    } else if (!missing(cols_before) && missing(cols_after)) {
      cols_after <- cols_before
    } else if (missing(cols_before) && missing(cols_after)) {
      cols_before <- grDevices::rainbow(n = nrow(m), alpha = 1)
      cols_after <- cols_before
    }
    image <- trans %*% m


    image_plot <-
      matador::plot_mat(image,
                        fix_coords = fix_coords,
                        color = cols_after,
                        linetype = linetype_after) + ggplot2::ggtitle("T(x)")

    if (nrow(m) != 2) {
      message("Cannot plot input matrix of dimension ", nrow(m))
      return(image_plot)
    }

    before_plot <-
      matador::plot_mat(m,
                        fix_coords = fix_coords,
                        color = cols_before,
                        linetype = linetype_before) + ggplot2::ggtitle("X")

    cowplot::plot_grid(before_plot, image_plot)
  }
