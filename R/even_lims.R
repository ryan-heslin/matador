#' Set Even Limits Based on Data Values
#'
#' @param vec A vector, typically a row or column represnting a dimension to plot
#'
#' @return A length 2 vector consisting of the negative and positive of the value of vec with the greatest absolute value.
#' @export
#'
#' @examples
#' # Create four-quadrant plot
#' library(ggplot2)
#'
#`p <- ggplot(mtcars, aes(x = hp, y = mpg)) +
#`  geom_point()+
#`  scale_x_continuous(limits = even_lims)+
#`  scale_y_continuous(limits = even_lims)
#`p
#`p + make_axes(p)
even_lims <- function(vec){
  c(-max(abs(vec)), max(abs(vec)))

}

library(ggplot2)



