#' Convert an R Matrix Object to LaTeX Code
#' @description This function converts an R matrix object into LaTeX code for rendering it in a LaTeX environment. It is intended for use in Rmarkdown,
#' as a convenience for translating calculations done in R code chunks into LaTeX.
#' @param m An R matrix object of any dimension
#' @param sink Logical determining output mode. If FALSE, the default, mat2latex prints the LaTeX code directly to console. If TRUE,
#' it returns the code without printing, a la R's sink function.
#' @return Latex code for printing the matrix. To render the code in an RMarkdown document, call the function in a chunk with  the `results = "asis"` option.
#' Alternately, set sink to FALSE, store the output in an object, and reference the object in an R code chunk.
#' @export
#'
#' @examples #Large matrices are not difficult to process.
#' mat2latex(as.matrix(mtcars))
#' #Make a list storing code to print each element of a matrix equation
#' A <- matrix(-1:2, nrow = 2)
#' B <- matrix(rep(0.5, 4), nrow =2)
#' C <- diag(x = 4, nrow =2)
# 'ABC <- matador::compose_trans(list(C, B, A))
# `mats <- lapply(list(C, B, A, ABC), matador::mat2latex, sink = TRUE)
mat2latex <- function(m, sink = FALSE){

  out <- apply(m, MARGIN = 1, function(x) paste(x, collapse = " & "))
  out[-length(out)] <- paste0(out[-length(out)], "\\\\")
  out <- c("\\begin{bmatrix}\n", out, "\n\\end{bmatrix}\n") %>%
    paste(collapse = " ")

  #Print output
  if(sink){
    return(out)
  }else{
  }
  cat(out)
}
