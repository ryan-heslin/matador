#' Convert a 2 x m Matrix to an m x 2 Tibble for Plotting
#'
#' @param m A 2 x m matrix.
#' @param col_names A character vector of two distinct column names for the
#' output. Defaults to c("x", "y"), which should cover most use cases.
#'
#'
#' @return A [tibble][tibble::tibble-package] organizing the matrix's values in
#'  plottable form, with columns _x_ and _y_ and one row for each column vector
#'   of the original matrix.
#' @export
#'
#' @examples tidy_mat(matrix(rnorm(n = 20, mean = 5, sd = 3), nrow = 2))
tidy_mat <- function(m, col_names = c("x", "y")) {
  # Return unmodified if already data.frame
  if (!is.matrix(m)){
    stop("Cannot tidy an object that is not a matrix")
  }else if (length(col_names) != 2){
    stop("Must provide 2 column names, not", length(col_names))
  }else if (dplyr::n_distinct(col_names) < 2){
    stop("Must provide unique column names")
  }

  suppressMessages(expr =  {
    split(m, seq(1, nrow(m))) %>%
      purrr::reduce(cbind) %>%
      tibble::as_tibble() %>%
      stats::setNames(col_names)
  })

}
