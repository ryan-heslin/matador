#' Plot The Vectors of a Matrix
#' @description This function plots the column vectors of a two-dimensional matrix.
#' @param m A 2 x _m_ matrix. Rows are assumed to represent dimensions.
#' @param color Color to use when drawing the matrix vectors. May be specified as a  hexadecimal.
#' @param fix_coords Logical determining the plot's aspect ratio. If TRUE, the plot is scaled to have an aspect ratio of 1,
#' meaning the  _x_ and _y_ dimensions are scaled exactly the same. Note this will distort the size of the plot and will
#' result in unreadable plots if the ranges of _x_ and _y_ are disparate.
#' Defaults to FALSE.
#' @return A ggplot object plotting each vector encoded in the provided matrix.
#' @export
#'
#' @examples plot_mat(rbind(-10:0, 0:10))
plot_mat <- function(m, color = "blue", fix_coords = FALSE) {
  if (nrow(m) != 2) {
    stop("Matrix of dimension ",
         nrow(m),
         ". Can only plot 2-dimensional matrix.")
  } else if (mode(m) != "numeric") {
    stop("Cannot plot matrix of mode ", mode(m))
  }
  #Set default color if invalid
  color <-
    ifelse(grepl("#", color) | color %in% colors(), color, "blue")

  m <- tidy_mat(m)

  xlim <- even_lims(m$x)
  ylim <- even_lims(m$y)

  #Use correct coord mode
  if(fix_coords){
    coord <- ggplot2::coord_fixed(ratio = 1, xlim = xlim, ylim = ylim)

  }else{
    coord <- ggplot2::lims(x = xlim, y = ylim)
  }

  out <- ggplot2::ggplot(data = m, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_segment(
      ggplot2::aes(
        xend = x,
        yend = y,
        x = 0,
        y = 0
      ),
      arrow =
        grid::arrow(type = "closed", length = grid::unit(.1, "inches")),
      arrow.fill = color,
      color = color,
      size = 1
    ) +
    coord

    out + make_axes(p = out)
}
