#' Draw X and Y Axes on a Plot
#' @description This function creates vertical and horizontal axes that may be added to a ggplot object. The intercepts for either or each may be specified.
#' Note the axes will not appear if they fall outside the limits of your plot.
#' This function is partly based on this StackOverflow post: https://stackoverflow.com/questions/17753101/center-x-and-y-axis-with-ggplot2
#' @param p A ggplot object
#' @param x x-intercept of the y-axis. Defaults to 0
#' @param y y-intercept of the x-axis. Defaults to 0.
#'
#' @return A list of four geom_segment objects, each corresponding to one half-axis, that may be added to a ggplot object.
#' @export
#'
#' @examples
make_axes <- function(p, x_int = 0, y_int = 0) {
  seg_params <-
    tibble::tibble(
      xend = c(-Inf, Inf, x_int, x_int),
      yend = c(y_int, y_int, Inf, -Inf),
      x = x_int,
      y = y_int
    )


  breaks <-
    list(
      x = tibble(
        xbreaks = ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x$breaks %>%
          {.[stats::complete.cases(.)]},
        y_int
      ),
      y = tibble(
        ybreaks = ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$breaks %>%
          {.[stats::complete.cases(.)]},
        x_int
      )
    )

  # Get widths for axis ticks and offset for labels
  x_off <- size_ticks(breaks$y$ybreaks)

  y_off <- size_ticks(breaks$x$xbreaks)

  # y goes before
  labs <-
    list(
      ggplot2::geom_segment(
        data = breaks$y,
        ggplot2::aes(
          x = x_int - y_off,
          xend = x_int + y_off,
          y = ybreaks,
          yend = ybreaks
        )
      ),
      ggplot2::geom_segment(
        data = breaks$x,
        ggplot2::aes(
          x = xbreaks,
          xend = xbreaks,
          y = y_int - x_off,
          yend = y_int + x_off
        )
      ),
      ggplot2::geom_text(
        data = breaks$y,
        ggplot2::aes(
          x = x_int - (2 *y_off),
          y = ybreaks,
          hjust =1,
          label = as.character(ybreaks)
        )
      ),
      ggplot2::geom_text(
        data = breaks$x,
        ggplot2::aes(
          x = xbreaks,
          y = y_int - (2 *x_off),
          label = as.character(xbreaks)
        )
      ),
      ggplot2::theme(
        axis.text = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank()
      )
    )


  lines <- purrr::pmap(
    seg_params,
    ggplot2::geom_segment,
    color = "black",
    arrow =
      grid::arrow(type = "closed", length = grid::unit(.1, "inches")),
    arrow.fill = "black",
    size = .5
  )
  append(lines, labs)

}
