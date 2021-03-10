#' Set Even Limits Based on Data Values
#' @description This function takes a numeric vector and returns a range of the negative and positive of the element with the highest absolute
#' value. It is intended to set even limits on plots when called on the breaks or data values corresponding to an aesthetic
#' @param vec A vector, typically a row or column representing a dimension to plot
#'
#' @return A length 2 vector consisting of the negative and positive of the value of vec with the greatest absolute value,
#'  respectively rounded down and up.
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
  c(-floor(max(abs(vec))), ceiling(max(abs(vec))))

}
