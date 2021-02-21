#' mat2latex
#' Converts an R matrix object to a representation in Latex code
#' @param m An R matrix object of any dimension
#'
#' @return Latex code for printing the matrix
#' @export
#'
#' @examples
mat2latex <- function(m){
  out <- apply(m, MARGIN = 1, function(x) paste(x, collapse = " & ") %>% paste0(., "\\\\"))
  cat("\\begin{bmatrix}\n")

  #Print code
  for (line in out){
    cat(line, "\n")
  }
  cat("\\end{bmatrix}\n")
}
