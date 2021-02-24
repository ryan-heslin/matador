#' Title
#'
#' @param m A 2 x m matrix.
#'
#' @return A tibble organiziang the matrix's values in plottable form, with columns _x_ and _y_ and one row for each column vector of the origianl matrix.
#' @export
#'
#' @examples
tidy_mat <- function(m){

  # Return unmodified if alredfy data.frame
  if (is.data.frame(m)){
    message("Received data.frame argument instead of matrix. Returning unmodified")
    return(m)
  }

  split(m, seq(1, nrow(m))) %>%
    purrr::reduce(cbind)
    tibble::as_tibble() %>%
    setNames(c("x", "y"))

}
