#' Plot the Lines of the Matrix of a System of Equations
#'
#' @param m An _n_ x 2 matrix representing a system of equations in n dimensions
#' @param b An _n_ x 1 matrix or atomic vector representing the solutions to the equations of m
#' @param colors Optional vector of _n_ distinct colors for the lines, one for each equation in m, reading downward. If not provided, colors are chosen automatically.
#'
#' @return A ggplot object depicting the lines of the equations.
#' @export
#'
#' @examples m1 <- matrix(c(1,3, 2, 4))
#' m2 <- matrix(1, 3, 2, 6)
#' # Consistent system
#' plot_lines(m= m1, b = c(2, 0))
#' # Inconsistent system
#' plot_lines(m = m2, b = c(5, 7))
#' # Linearly dependent system - only one line appears because there are infinitely many solutions
#' plot_lines(m = m2, b = c(1, 2))
#' Plot many equations at once
#' m3 <- matrix(sample(-10:10, 10), nrow = 5)
#' plot_lines(m3, b = -2:2)
plot_lines <-
  function(m, b, colors = grDevices::rainbow(n = nrow(m))) {
    browser()
    # Get intercepts and slopes
    params <-
      tibble::tibble(x = parmas[1, ], y = params[, 2], b = b) %>%
      dplyr::transmute(slope = -x / y, intercept = ifelse(y = 0, b / x, b /
                                                            y))

    m <- setNames(as.data.frame(m), nm = c("x", "y"))

    # Plot
   ggplot2::ggplot(data = m, ggplot2::aes(x = x, y = y)) +
      #ggplot2::geom_point(alpha = .5) +
      ggplot2::geom_abline(
        slope = params$slope,
        intercept = params$intercept,
        color = colors
      )
  }
