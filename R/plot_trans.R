#' plot_transform
#'
#' @param m
#' @param trans
#' @param before
#' @param after
#'
#' @return
#' @export
#'
#' @examples
plot_transform <- function(m, trans, before = "blue", after = "red"){

  image <- m %*% trans

  p1 <- matador::plot_trans(m, color = before)
  p2 <- matador::plot_mat(image, color = after)

  gridExtra::grid.arrange(p1, p2, ncol = 2)
}
