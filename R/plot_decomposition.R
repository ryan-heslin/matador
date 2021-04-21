plot_decomposition <- function(m) {
  if(!is.matrix(m)) stop("Argument must be a matrix")
  if(!is.numeric(m)) stop("Argument must be a numeric matrix")
  if(!identical(dim(m), c(2L, 2L))) stop("Argument must be a 2 x 2 matrix")
  if(det(m) == 0) stop("Argument has rank < 2 and is not a valid basis")


  u1 <- normalize(m[,1])
  v2_perp <- m[,2] - (dot(u1, v2)) * u1

  v2_par <- m[,2] - v2_perp
  u2 <- normalize(v2_parallel)

  #Plots
  p1 <- plot_mat(m)
  p2 <- plot_mat(cbind(u1, m[,2]))
  p3 <- p2 +
    geom_segment(aes(xend = !!u1[1], yend = !!u2[2],
                     x = !!v2par[1], y = !!v2par[2]), color = "blue") +
    geom_segment(aes(xend = !!v2_par[1], yend = !!v2_par[2], x = v2[1], y = !!v2[2] ), color = "red") +
    coord_cartesian()

  p4 <- plot_mat(cbind(u1, u2), fix_coords = TRUE)

  gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2, ncol =2)
}
