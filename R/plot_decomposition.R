#' Graphically Display the Orthogonal Decomposition of a Two-Dimensional Matrix
#'
#' @description This function produces plots illustrating the transformation of
#' a pair of linearly independent two-dimensional vectors into an orthogonal
#' basis (the `Q` of the `QR` decomposition). The algorithm, which can be found
#' in any linear algebra textbook,
#' @param m A 2 x 2 numeric matrix of full rank, one whose columns provide a
#' valid basis for the two-dimensional plane. If `m` has different dimesnions,
#' a different type, or does not have full rank, an error is thrown.
#' @param color_par Color for the second vector's parallel component.
#' @param color_perp Color for the second vector's orthogonal component.
#' @param output Output mode. If "plot", a single `gridExtra` multiplot featuring
#' the plots side by side. If "list", a list containing the plots. Defaults to
#' "plot."
#' @param fix_coords Logical determining whether to draw the plots with fixed
#' aspect ratio. Defaults to FALSE.
#'
#' @return If `output` is "plot", a multiplot featuring the decomposition plots;
#' otherwise a list containing them.
#' @export
#'
#' @examples
#' plot_decomposition(square(1, 4, 7, 2))
#' # Already orthogonal vectors: only scaling necessary
#' plot_decomposition(square(30, 20, 2, -3))
plot_decomposition <-
  function(m,
           color_par = "blue",
           color_perp = "red",
           output = c("list", "plot"),
           fix_coords = FALSE) {
    if (!is.matrix(m))
      stop("Argument must be a matrix")
    if (!is.numeric(m))
      stop("Argument must be a numeric matrix")
    if (!identical(dim(m), c(2L, 2L)))
      stop("Argument must be a 2 x 2 matrix")
    if (det(m) == 0)
      stop("Argument has rank < 2 and is not a valid basis")
  #browser()
    output <- match.arg(output)
    if (is.na(output))
      output <- "plot"

    u1 <- normalize(m[, 1])
    v2_perp <- m[, 2] - (dot(u1, m[, 2])) * u1

    v2_par <- m[, 2] - v2_perp
    u2 <- normalize(v2_perp)
    #browser()
    # Paramters for drawing perp and parallel
    segs <-
      tibble(
        xend = c(u1[1], v2_par[1]),
        yend = c(u1[2], v2_par[2]),
        x = c(v2_par[1], m[1, 2]),
        y = c(v2_par[2], m[2, 2]),
        color = c(color_par, color_perp),
        lineend = "round"
      ) %>%
      purrr::pmap(ggplot2::geom_segment)

      # Offset to ensure end of v_par appears on plot
    seg_offset <- 1 + sqrt(max(abs(v2_par))/max(abs(m[,2])))

    #Parameters for using draw_plots to create plots
    params <-
      list(
        m = list(m,   cbind(u1, m[, 2]), cbind(u1, m[, 2]), cbind(u1, u2)),
        labels = list(
          c("v[1]", "v[2]"),
          c("u[1]", "v[2]"),
          c("", ""),
          c("u[1]", "u[2]")
        ),
        fix_coords = as.list(rep(fix_coords, 4)),
        lims_scale = as.list(c(1.125, 1.125, seg_offset, 1.125))
      ) %>%
      purrr::transpose()
    plots <- purrr::map(params,
                        ~ do.call("draw_plots", .x)) %>%
      setNames(paste0("p", 1:4))

    plots$p3 <- plots$p3 + segs

    if(output == "list"){
      return(unname(plots))
    }else{
      do.call(getExportedValue("gridExtra", "grid.arrange"),
            c(plots, nrow = 2, ncol = 2))
    }
  }

# Helper function to render each plot
draw_plots <- function(m, labels, fix_coords, lims_scale) {
  plot_mat(m, fix_coords = fix_coords, lims_scale = lims_scale) +
    ggplot2::geom_text(ggplot2::aes(x = xend, y = yend + max(abs(yend)) /
                                      8),
                       label = labels,
                       parse = TRUE)

}
