plot_transform <- function(m, trans){

  mat <- m %*% trans() %>%
    t() %>%
    as_tibble() %>%
    set_names(c("x", "y"))

  m <- m %>% t() %>%
    as_tibble() %>%
    set_names(c("x", "y"))

  #seg <- purrr::partial("geom_segment",  )
  p1 <- ggplot(data = m) +
    geom_segment(aes(xend = x, yend = y, x = 0, y =0), arrow =
                   arrow(type = "closed", length = unit(.1, "inches")),
                 arrow.fill = "blue", color = "blue", size = 1) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0)

  #TODO Latex parsing
  p2 <- ggplot(data = mat) +
    geom_segment(aes(x = 0, y =0, xend = x, yend = y), arrow =
                   arrow(type = "closed", length = unit(.1, "inches")),
                 arrow.fill = "red", color = "red", size = 1) +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0)


  grid.arrange(p1, p2, ncol = 2)
}
