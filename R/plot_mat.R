#' Title
#'
#' @param m A two-row matrix with any number of columns. Rows are assumed to represent dimensions
#' @param color Color to use when drawing the matrix vectors. may be hexadecimal.
#'
#' @return A ggplot object plotting each vector encoded in the provided matrix
#' @export
#'
#' @examples plot_mat(rbind(-10:0, 0:10))
plot_mat <- function(m, color = "blue") {
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
    ggplot2::scale_x_continuous(limits = even_lims) +
    ggplot2::scale_y_continuous(limits = even_lims)


    out + make_axes(p = out)
}
