% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_transform.R
\name{plot_transform}
\alias{plot_transform}
\title{Plot a Matrix Before and After a Linear Transformation}
\usage{
plot_transform(
  trans,
  m = diag(nrow = 2),
  cols_before,
  linetype_before = "solid",
  cols_after,
  linetype_after = "dashed",
  fix_coords = FALSE
)
}
\arguments{
\item{trans}{A 2 x m matrix representing the transformation to apply to m, for which multiplication by m is defined.
Alternately, a list of matrices for which multiplication is defined in the conventional
order. If such a list is passed, the leftmost matrix (the final transformation) must have two dimensions.}

\item{m}{A matrix of any dimension for which multiplication by trans (or, if trans is a list, its composition) is defined.
By default, the 2 x 2 identity.}

\item{cols_before, linetype_before, cols_after, linetype_after}{Character vectors dictating the \code{color} and \code{linetype} aesthetics
of \code{geom_segment}, used to draw each plot. All are passed on to \code{plot_mat}
via its \code{...} argument and evaluated there. By default, this function uses
color to distinguish individual vectors and linetype to distinguish the stage
of the transformation, which fits most use cases. Accordingly, if one of
\code{cols_before} and \code{cols_after} is missing, the other is assigned the
value of the one that was supplied; if both are missing, they are assigned
the same contrasting colors.}

\item{fix_coords}{Passed on tho plot_mat. Logical determining whether to
force an even aspect ratio on the plots. Defaults to FALSE.}
}
\value{
A side-by-side plot, the first panel depicting the original matrix, the second the matrix after its transformation.
If _m_is not two-dimensional, only
a plot of the image of the transformation.
}
\description{
This function plots the vectors within a matrix, multiplies the matrix by the provided transformation,
then plots the result. It makes it easy to visualize even complex linear transformations on the two-dimensional plane.
}
\examples{
# 45-degree rotation counterclockwise
plot_transform(m = matrix(c(3, 1, 1, 2), nrow =2),
trans = matrix(c(1/sqrt(2), 1/sqrt(2), -1/sqrt(2), 1/sqrt(2)), nrow =2))
# Projection of unit cube onto xy plane
plot_transform(m = diag(nrow =3), trans= matrix(c(1, 0, 0, 1, .5, .5), nrow =2))

}
\seealso{
\link[matador]{plot_mat}, the function used to render the plots.
}
