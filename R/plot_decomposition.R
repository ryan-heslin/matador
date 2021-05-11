#' Graphically Display the Orthogonal Decomposition of a Two-Dimensional Matrix
#'
#' @description This function produces plots illustrating the transformation of
#' a pair of linearly independent two-dimensional vectors into an orthogonal
#' basis (the `Q` of the `QR` decomposition). The algorithm, which can be found
#' in any linear algebra textbook, scales the first vector to unit length, then
#' extracts the component of the second vector orthogonal to the first and
#' normalizes it. The plots are based on those found on pp. 218-219 of Otto
#' Bretscher's _Linear Algebra with Applications_ (5th ed.).
#' @param m A 2 x 2 numeric matrix of full rank, one whose columns provide a
#' valid basis for the two-dimensional plane. If `m` has different dimensions,
#' a different type, or does not have full rank, an error is thrown.
#' @param color_par Color for the second vector's parallel component.
#' @param color_perp Color for the second vector's orthogonal component.
#' @param color_text Color for the text of the equations.
#' @param output Output mode. If "plot", a single `gridExtra` multiplot featuring
#' the plots side by side. If "list", a list containing the plots. Defaults to
#' "plot."
#' @param fix_coords Logical determining whether to draw the plots with fixed
#' aspect ratio. Defaults to FALSE.
#'
#' @return If `output` is "plot", a multiplot featuring the six decomposition plots;
#' otherwise a list containing them.
#' @export
#'
#' @examples
#' plot_decomposition(square(1, 4, 7, 2))
#' # With custom colors
#' plot_decomposition(square(1:4), color_par = "green", color_perp = "orange")
#' # Already orthogonal vectors: only scaling necessary
#' plot_decomposition(square(30, 20, 2, -3))
plot_decomposition <-
  function(m,
           color_par = "blue",
           color_perp = "red",
           color_text = "red4",
           output = "plot",
           fix_coords = FALSE) {
    if (!is.matrix(m))
      stop("Argument must be a matrix")
    if (!is.numeric(m))
      stop("Argument must be a numeric matrix")
    if (!identical(dim(m), c(2L, 2L)))
      stop("Argument must be a 2 x 2 matrix")
    if (det(m) == 0)
      stop("Argument has rank < 2 and is not a valid basis")

    if (output != "plot") {
      output <- "list"
    }

    u1 <- normalize(m[, 1])
    v2_perp <- m[, 2] - (dot(u1, m[, 2])) * u1
    v2_par <- m[, 2] - v2_perp
    u2 <- normalize(v2_perp)

    # Parameters for drawing perp and parallel
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
    seg_offset <- 1 + sqrt(max(abs(v2_par)) / max(abs(m[, 2])))
    lims_scales <- c(rep(1.125, 2), seg_offset, rep(1.125, 3))

    # Labels for parallel segments
    seg_lab_pos <-
      list(par = u1 + (v2_par - u1) / 2,
           perp = v2_par + (m[, 2] - v2_par) / 2)

    #Parameters for using draw_plots to create plots
    params <-
      list(
        m = list(
          m,
          cbind(u1, m[, 2]),
          cbind(u1, m[, 2]),
          cbind(u1, v2_perp),
          cbind(u1, v2_perp),
          cbind(u1, u2)
        ),
        labels = list(
          c("v[1]", "v[2]"),
          c("u[1]", "v[2]"),
          c("u[1] == over(1, '||' * v[1] * '||') * v[1]", "v[2]"),
          c("u[1]", "v[2]"),
          c("v[2]^{perp} == v[2] - (u[1] %.% v[2]) * u[1]", "v[2]"),
          c("u[1]", "u[2] == over(1, '||' * v[2]^{perp} * '||') * v[2]^{perp}")
        ),
        fix_coords = as.list(rep(fix_coords, 6)),
        lims_scale = as.list(lims_scales),
        color_text = as.list(rep(color_text, 6))
      ) %>%
      purrr::transpose(.)
    plots <- purrr::map(params,
                        ~ do.call("draw_plots", .x)) %>%
      stats::setNames(nm = paste0("p", 1:6))

    plots$p3 <- plots$p3 + segs +
      ggplot2::annotate(
        "text",
        x =  seg_lab_pos$par[1],
        y = seg_lab_pos$par[2],
        label = "v[2]^{perp}",
        color = color_text,
        parse = TRUE
      ) +
      ggplot2::annotate(
        "text",
        x =  seg_lab_pos$perp[1],
        y = seg_lab_pos$perp[2],
        label = "v[2]^{parallel}",
        color = color_text,
        parse = TRUE
      )

    if (output == "list") {
      return(unname(plots))
    } else{
      do.call(getExportedValue("gridExtra", "grid.arrange"),
              c(plots, nrow = 3, ncol = 2))
    }
  }

# Helper function to render each plot
draw_plots <-
  function(m,
           labels,
           fix_coords,
           lims_scale,
           color_text) {
    #blanks to ensure text statys in plot
    plot <-
      plot_mat(m,
               fix_coords = fix_coords,
               lims_scale = lims_scale,
               linetype = "dashed") +
      ggplot2::geom_blank(ggplot2::aes(x = xend, y = (yend + max(abs(
        yend
      )) /
        6) * 2)) +
      ggplot2::geom_blank(ggplot2::aes(x = (xend + max(abs(
        xend
      )) / 6) * 2)) +
      ggplot2::geom_blank(ggplot2::aes(x = -(xend + max(abs(
        xend
      )) / 6) * 2)) +
      ggplot2::geom_text(
        ggplot2::aes(x = xend, y = yend + max(abs(yend)) /
                       6),
        label = labels,
        parse = TRUE,
        color = color_text
      )

  }
