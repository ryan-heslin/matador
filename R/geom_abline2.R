#' Process Parameters for both Vertical and Non-Vertical Lines
#'
#' @param slope Argument to geom_abline. If infinite, passed to geom_vline instead.
#' @param intercept Argument to geom_abline. Treated as geom_vline's x-intercept argument if slope is infinite.
#' @param color Argument to either function
#'
#' @return a geom_abline or geom_vline object, depending on the arguments used
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
