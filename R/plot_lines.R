#' plot_lines
#'
#' @param m An n x 2 matrix representing a system of equations in n dimensions
#' @param b An n x 1 matrix or atomic vector representing the solutions to the equations of m
#' @param colors Optional vector of n distinct colors, one for each equation in m, reading downward. If not provided, colors are chosen automatically
#'
#' @return
#' @export
#'
#' @examples
plot_lines <-
  function(m, b, colors = grDevices::rainbow(n = nrow(m))) {
    browser()
    # Get intercepts and slopes
    params <- cbind(m, b) %>% apply(MARGIN = 1, function(x)
      c(-x[1] / x[2], ifelse(x[2] == 0, x[3] / x[1], x[3] / x[2]))) %>% #Divide by x coef if y coef is 0 (vertical line) -x intercept in latter case
      t() %>%
      as.data.frame()
    names(params) <- c("slope", "intercept")

    m <- setNames(as.data.frame(m), nm = c("x", "y"))

    # Plot
    out <- ggplot2::ggplot(data = m, ggplot2::aes(x = x, y = y)) +
      #ggplot2::geom_point(alpha = .5) +
      ggplot2::geom_abline(
      slope = params$slope,
      intercept = params$intercept,
      color = colors
    )
    out
  }
