#' Draw X and Y Axes on a Plot
#' @description This function creates vertical and horizontal axes that may be added to a ggplot object. The intercepts for either or each may be specified.
#' Note the axes will not appear if they fall outside the limits of your plot.
#' @param x x-intercept of the y-axis, 0 by default
#' @param y y-intercept of the x-axis, 0 by default
#'
#' @return A list of four geom_segment objects, each corresponding to one half-axis, that may be added to a ggplot object.
#' @export
#'
#' @examples
make_axes <- function(x_int = 0, y_int = 0, ticks_width = 20) {

  seg_params <-
    tibble::tibble(
      xend = c(-Inf, Inf, x, x),
      yend = c(y, y, Inf, -Inf),
      x = x,
      y = y
    )

  breaks <- tibble(xbreaks = p$layout$panel_params[[1]]$x$breaks,
  ybreaks = p$layout$panel_params[[1]]$y$breaks)

  # Get widths for axis ticks and offset for labels
  x_off <- max(abs(ybreaks)/20) + y_int

  y_off <- max(abs(xbreaks)/20) + x_int

  x_nudge <- 0
  y_nudge <- 0


  labs <- list(ggplot2::geom_segment(data = breaks, aes(x = -x_int - y_off, xend = y_off, y = ybreaks, yend=ybreaks)),
       ggplot2::geom_segment(data = breaks, aes(x = xbreaks, xend = xbreaks, y = y_int - x_off, yend= x_off)),
       ggplot2::geom_text(data=breaks, aes(x =x_int - x_nudge, y = ybreaks, label = as.character(ybreaks))),
       ggplot2::geom_text(data=breaks, aes(x =xbreaks - x_nudge, y = x_int, label = as.character(xbreaks))),
  )


  lines <- purrr::pmap(
    params,
    ggplot2::geom_segment,
    color = "black",
    arrow =
      grid::arrow(type = "closed", length = grid::unit(.1, "inches")),
    arrow.fill = "black",
    size = 1
  )
  append(lines, labs)

}

