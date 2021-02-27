#' Convert a 2 x m Matrix to a m x 2 Tibble for Plotting
#'
#' @param m A 2 x m matrix.
#'
#' @return A [tibble][tibble::tibble-package] organizing the matrix's values in plottable form, with columns _x_ and _y_ and one row for each column vector of the origianl matrix.
#' @export
#'
#' @examples tidy_mat(matrix(rnorm(n = 20, mean = 5, sd = 3), nrow = 2))
tidy_mat <- function(m) {
  # Return unmodified if alredfy data.frame
  if (is.data.frame(m)) {
    message("Received data.frame argument instead of matrix. Returning unmodified")
    return(m)
  }

  suppressMessages(expr =  {
    split(m, seq(1, nrow(m))) %>%
      purrr::reduce(cbind) %>%
      tibble::as_tibble() %>%
      setNames(c("x", "y"))
  })

}
