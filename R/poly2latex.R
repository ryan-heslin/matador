#' Convert a Numeric Vector to LaTeX code for a Polynomial Expression
#'
#' @description This function transforms a numeric vector representing a
#' polynomial in polynomial space into LaTeX code to render that polynomial. Such
#' vectors represent linear combinations of coefficient and unknown terms, which
#' form the basis of the space. This function is intended for use in Rmarkdown
#' documents with PDF output, where it can convert vectors into formatted
#' equations automatically.
#'
#' @param v A numeric vector of arbitrary length
#' @param sym A character to represent the unknown in the polynomial expression.
#' Defaults to "t"
#' @param dir Direction in which the terms of the polynomial represented by the
#' vector increase. For "forward", the default, the constant term is the leftmost,
#' for "backward" the rightmost
#' @param lhs Value to use to represent the left-hand side of the equation.
#' Defaults to `f(t) = ` Set to "" or NULL to obtain only the right-hand side.
#' @param sink Logical determining whether to return the output conventionally or
#' print it directly to console. Defaults to FALSE.
#'
#' @return
#' @export
#'
#' @examples poly2latex(c(1:5))
#' #Constant function
#' poly2latex(-7)
#' poly2latex(round(rnorm(5), digits = 2)
poly2latex <- function(v,
                       sym = "t",
                       dir = "forward",
                       lhs = paste0("f(", sym, ") = "),
                       sink = FALSE) {
  validate_vec(v)
  # Reverse if need be
  if (dir != "forward") {
    v <- rev(v)
  }
  syms <- c("", rep(sym, times = max(length(v) - 1, 0)))

  #Exponents only degree of 2 or higher
  if (length(v > 2)) {
    syms[-c(1:2)] <- paste0(syms[-c(1:2)], "^ {", 2:(length(v) - 1), "}")
  }
  eqn <- paste0(v, letters[seq_along(v)], syms) #drop 0s so clean_eqn doesn't break
  eqn <- eqn[!grepl("^0[a-z]", x = eqn)]

  out <- paste0(lhs, clean_eqn(eqn))
  if (sink) {
    return(out)
  }
  print_eqn(out)
}
