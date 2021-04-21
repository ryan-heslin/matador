#' Plot The Vectors of a Matrix
#' @description This function plots the column vectors of a two-dimensional matrix.
#' @param m A 2 x _m_ matrix. Rows are assumed to represent dimensions.
#' @param fix_coords Logical determining the plot's aspect ratio. If TRUE, the plot is scaled to have an aspect ratio of 1,
#' meaning the  _x_ and _y_ dimensions are scaled exactly the same. Note this will distort the size of the plot and will
#' result in unreadable plots if the ranges of _x_ and _y_ are disparate.
#' Defaults to FALSE.
#' @param ... Vectors of aesthetics to guide how `geom_segment` draws the plot:
#'  `color`, `alpha`, `linetype`,
#' and `size`. Default to `"black"`, 1, `"solid"`, and 1, respectively.
#' An error is thrown if any argument of `...` has a different name,
#' or if a valid name is used more than once. These arguments are not
#' validated; if invalid values are chosen, `geom_segment` will throw an error
#' when it attempts to interpret them. If less than or greater than _m_, recycled
#' or truncated by R's standard rules.
#' @details @param color governs the color of the arrowhead plotted at the tip of each
#' vector, and @param size determines the size.
#' @return A ggplot object plotting each vector encoded in the provided matrix.
#' @export
#'
#' @examples plot_mat(rbind(-10:0, 0:10))
#' # Very ugly plot of the standard vectors
#' plot_mat(diag(nrow =2), size = c(2, 3), color = c("purple", "orange"))
plot_mat <- function(m,
                     fix_coords = FALSE, ...) {
  SEGMENT_AES <-
    list(
      alpha = 1,
      color = "black",
      linetype = "solid",
      size = 1
    )
  dots <- list(...)
  if (nrow(m) != 2) {
    stop("Matrix of dimension ",
         nrow(m),
         ". Can only plot 2-dimensional matrix.")
  } else if (mode(m) != "numeric") {
    stop("Cannot plot matrix of mode ", mode(m))
  } else if (any(!names(dots) %in%  names(SEGMENT_AES))) {
    stop(
      "Argument passed via ... does not match a geom_segment aesthetic
         with a corresponding scale_*_identity function"
    )
  } else if (dplyr::n_distinct(names(dots)) != length(names(dots))) {
    stop("Cannot pass multiple specifications for the same aesthetic via ...")
  }

  # Substitute default aesthetics
  dots <-
    c(dots, SEGMENT_AES[setdiff(names(SEGMENT_AES), names(dots))])

  m <- tidy_mat(m, col_names = c("xend", "yend")) %>%
    dplyr::mutate(x = 0, y = 0)
  if (length(dots) != 0)
    m <-
    cbind(m, purrr::map_dfc(dots, ~ rep_len(.x, length.out = nrow(m))))

  xlim <- even_lims(m$xend)
  ylim <- even_lims(m$yend)

  #Use correct coord mode
  if (fix_coords) {
    coord <- ggplot2::coord_fixed(ratio = 1,
                                  xlim = xlim,
                                  ylim = ylim)

  } else{
    coord <- ggplot2::coord_cartesian(xlim = xlim, ylim = ylim)
  }
  out <- suppressMessages(ggplot2::ggplot(data = m) +
    ggplot2::geom_segment(
      x = 0,
      y = 0,
      ggplot2::aes_all(names(m)),
      arrow = grid::arrow(
        type = "closed",
        angle = 20,
        length = grid::unit(.1 * m$size, "inches")
      ),
      arrow.fill = m$color

    ) +
    ggplot2::scale_continuous_identity(aesthetics = c("alpha", "size")) +
    ggplot2::scale_discrete_identity(aesthetics = c("linetype", "color")) +
    coord)

  out + make_axes(p = out)
}
