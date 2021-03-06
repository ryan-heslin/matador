#' Process Parameters for both Vertical and Non-Vertical Lines
#'
#' @param slope
#' @param intercept
#' @param color
#'
#' @return
#'
#'
#' @examples
geom_abline2 <- function(slope, intercept, color){
  if(!is.finite(slope)){
    return(ggplot2::geom_vline(xintercept = intercept, color = color))
  }else{
    return(ggplot2::geom_abline(slope=slope, intercept=intercept, color = color))
  }
}
