#' Convert an R Matrix Object to LaTeX Code
#' @description This function converts an R matrix object into LaTeX code for
#'  rendering in a LaTeX environment. It is intended for use in Rmarkdown,
#' as a convenience for translating calculations done in R code chunks into LaTeX.
#' @param m An R matrix object of any dimension. If not a matrix, the function
#' attempts to coerce it to one using `as.matrix`, which should work on ordinary
#' vectors but may have unexpected results on other objects.
#' @param sink Logical determining output mode. If FALSE, the default, mat2latex
#' prints the LaTeX code directly to console. If TRUE,
#' it returns the code without printing, a la R's sink function.
#' @param slash_repeats Number of times to repeat the "\\" escape. This should
#' usually be 1, but should be increased if the generated LaTeX code will be
#' parsed twice, as will happen if `mat2latex` is invoked in an RStudio snippet.
#' Defaults to 1.
#' @return Latex code for printing the matrix. To render the code in an Rmarkdown
#' document, call the function in a chunk with  the `results = "asis"` option.
#' Alternately, set sink to TRUE, store the output in an object, and call `cat`
#' or `print_eqn` on the object in an R code chunk.
#' @export
#'
#' @examples #Large matrices are not difficult to process.
#' mat2latex(as.matrix(mtcars))
#' # Make a list storing code to print each element of a matrix equation
#' A <- matrix(-1:2, nrow = 2)
#' B <- matrix(rep(0.5, 4), nrow =2)
#' C <- diag(x = 4, nrow =2)
#' ABC <- matador::compose_trans(list(C, B, A))
#' mats <- lapply(list(C, B, A, ABC), matador::mat2latex, sink = TRUE)
#' # Some trickery is required to print matrices stored in a list.
#' invisible(sapply(mats, function(x) cat(x, sep = "\n")))
mat2latex <- function(m, sink = FALSE, slash_repeats =1) {
  if (!is.matrix(m)) {
    message("Argument is not a matrix. Attempting to coerce")
    m <- as.matrix(m)
  }
  slashes <- paste(rep("\\", times = slash_repeats), collapse = "")
  out <-
    apply(m, MARGIN = 1, function(x)
      paste(x, collapse = " & "))
  out[-length(out)] <- paste0(out[-length(out)], paste0(rep(slashes, times = 2), collapse = ""))
  out <- c(paste0(slashes, "begin{bmatrix}"), out, paste0(slashes, "end{bmatrix}"))

  #Print output
  if (sink) {
    return(out)
  } else{

  }
  print_eqn(out)
}
