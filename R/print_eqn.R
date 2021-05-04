#' Print an Equation for LaTeX Rendering
#' @description This simple function takes a a character vector representing an
#' equation, matrix, or other text interpretable by a LaTeX math environment and
#' outputs it to console. It is used by `mat2latex` and `poly2latex` to produce
#' LaTeX-readable code from R objects.
#' This function builds on \href{https://stackoverflow.com/questions/30879083/make-r-markdown-code-blocks-into-math-mode}{this} StackOverflow post.
#' @param eqn A character vector representing LaTeX code that may be interpreted
#' by a math environment.
#' @param delim Delimiter for the printed code. Defaults to "$$", the TeX standard,
#' which should suit most uses.
#' @param file Passed to `cat`'s file argument, with `append = FALSE`. Defaults
#' to "" (i.e., print directly to standard output).
#'
#' @return The object printed to console, or appended to a file if `file` is
#' set.
#' @export
#'
#' @examples
#' # A polynomial transformation - note the escape backslashes!
#' # Try this in an rmarkdown chunk with `results = asis`
#' eqn <- c("\\begin{aligned}", "& T(f) = f(3) + f''\\\\",
#' "& = a + 3b + 9c +b + 2ct", "\\end{aligned}")
#' print_eqn(eqn)
print_eqn <- function(eqn, delim = "$$", file = ""){
  cat(delim, eqn, delim, sep = "\n", file = file, append = TRUE)
}
