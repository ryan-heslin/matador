#' Compute Spans for Ticks on Custom Axes
#'
#' @param breaks Vector of breaks for a ggplot object
#' @param span  Proportion of the range of the breaks the ticks should span in each direction
#'
#' @return Value representing the distance left or right the tick should span from the drawn axis
#'
#'
#'
size_ticks <- function(breaks, span = .0125){
  (max(breaks, na.rm = TRUE)-min(breaks, na.rm = TRUE)) * span
}
