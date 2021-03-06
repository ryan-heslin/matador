#' Plot the Lines of the Matrix of a System of Equations
#'
#' @param m An _n_ x 2 matrix representing a system of equations in n dimensions
#' @param b An _n_ x 1 matrix or atomic vector representing the solutions to the equations of m. If omitted, a vector of n 0s isused, as in a homoegeneous system of equations
#' @param colors Optional vector of colors for the lines, one for each equation in m, reading downward. If not provided, colors are chosen automatically. If shorter than b,
#' it is recycled by R's usual rules. If longer,extra colors are ignored.
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
  function(m,
           b = rep(0, nrow(m)),
           colors = grDevices::rainbow(n = nrow(m))) {


    if (mode(m) != "numeric" | mode(b) != "numeric") {
      stop("Inputs of nuon-numeric type.")
    } else if (ncol(m) != 2) {
      stop("Cannot plot equations of", ncol(m), "dimension.")
    } else if (length(b) != nrow(m)) {
      stop("Dimensions of inputs disagree: m has",
           nrow(m),
           "dimensions; b has",
           nrow(b),
           ".")
    }

    # Get intercepts and slopes
    params <-
      tibble::tibble(x = m[, 1], y = m[, 2], b = as.vector(b)) %>%
      dplyr::transmute(slope = -x / y,
                       intercept = dplyr::if_else(y == 0, b / x, b /
                                                    y), color = colors[1:length(b)])

    m <- setNames(as.data.frame(m), c("x", "y"))

    # Annoying vertical line special case
    if (all(is.finite(params$slope))){
      xlim <- even_lims(m$x)
    }else{
      xlim <- even_lims(c(m$x, b))
    }
    ylim <- even_lims(m$y)

    # Plot
    out <- ggplot2::ggplot(data = m, ggplot2::aes(x = x, y = y)) +
      #ggplot2::geom_point(alpha = .5) +
      purrr::pmap(params, geom_abline2) +
      ggplot2::xlim(xlim) +
      ggplot2::ylim(ylim)

    out + make_axes(out)
  }
