#' Convert an R Matrix Object to LaTeX Code
#'
#' @param m An R matrix object of any dimension
#'
#' @return Latex code for printing the matrix. To render the code in an RMarkdown document, call the funciton in a chunk with  the `results = "asis"` option.s
#' @export
#'
#' @examples #Large matrices are not difficult to process.
#' mat2latex(as.matrix(mtcars))
mat2latex <- function(m){
  out <- apply(m, MARGIN = 1, function(x) paste(x, collapse = " & ") %>% paste0(., "\n"))


  #PRint output
  cat("\\begin{bmatrix}\n")
  cat(out)
  cat("\\end{bmatrix}")
  print("\\")
}
