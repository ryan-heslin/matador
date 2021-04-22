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

    output <- match.arg(output)
    if (is.na(output))
      output <- "plot"

    coord_fun <-
      if (fix_coords)
        ggplot2::coord_fixed
    else
      ggplot2::coord_cartesian

    u1 <- normalize(m[, 1])
    v2_perp <- m[, 2] - (dot(u1, m[, 2])) * u1

    v2_par <- m[, 2] - v2_perp
    u2 <- normalize(v2_perp)

    m2 <- cbind(u1, m[, 2])
    m4 <- cbind(u1, u2)

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

    # TODO: Figure out limits to keep labels within plot

    # Expand limits to show projection point
    xlim <- even_lims(c(v2_par[1], m[1,]))
    ylim <- even_lims(c(v2_par[2], m[2,]))

    #Plots
    p1 <-
      plot_mat(m, fix_coords = fix_coords) +
      ggplot2::coord_cartesian(ylim = c(-7 - ((1 / 8) * 7),
                      7 + ((1 / 8) * 7))) +
      ggplot2::geom_text(
        ggplot2::aes(x = xend,
                     y = yend + (1 / 8) * max(abs(yend))),
        # improvised vjust
        label = c("v[1]", "v[2]"),
        ,
        parse = TRUE
      )


    p2 <- plot_mat(m2, fix_coords = fix_coords) +
      ggplot2::geom_text(
        ggplot2::aes(x = xend,
                     y = yend + (1 / 8) * max(abs(yend))),
        label = c("u[1]", "v[2]"),
        parse = TRUE
      )
    p3 <- suppressMessages(plot_mat(m2, fix_coords = fix_coords) +
                             segs +
                             coord_fun(xlim = xlim, ylim = ylim))

    p4 <- plot_mat(m4, fix_coords = fix_coords) +
      ggplot2::geom_text(
        ggplot2::aes(x = xend,
                     y =  yend + (1 / 8) * max(abs(yend))),
        label = c("u[1]", "u[2]"),
        parse = TRUE
      )

    gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
  }
