#' Extract and Modify Values in an Expression
#'
#' @description This function extracts symbols from an expression and
#' evaluates them in a manner the user specifies. First, each symbol
#' is evaluated in the supplied `envir`. If it is defined in that environment
#' and is not bound to a function, `extract_values` then applies `pred` to
#' the value. If `pred` returns `TRUE`, the value is then transformed by
#' `fun`.
#' @param exp A quoted expression.
#' @param env An environment in which to evaluate values in the expression.
#' Defaults to the caller environment.
#' @param pred A predicate function that returns a length-one logical when
#' applied to evaluated values within the expression. Defaults to `TRUE`.
#' @param fun An optional function to apply to each evaluated value.
#' `extract_values` will return whatever this function returns for each value
#' successfully evaluated in `env`. Defaults to `identity` - which leaves the
#' value unchanged.
#' @param ... Additional arguments to `fun`.
#'
#' @return A named vector or list of the successfully evaluated values.
#' If none were successfully evaluated, an empty list.
#'
#' @export
#'
#' @examples
#'  envir <- new.env()
#'  envir$A <- square(1:9)
#'  B <- square(4, 5, 6, 7)
#'  k <- 2
#'  extract_values(quote(A + B), env = envir, pred = function(x) nrow(x) >2, fun = mean)
#'  extract_values(quote(k * A), pred = is.matrix, fun = function(x) x %^% 3)
#'
extract_values <- function(exp, env, pred = function(val) TRUE, fun = identity, ...) {

  if(missing(env)){
    env <- parent.frame()
  }
  out <- sapply(all.names(exp), function(sym) {
    val <-
      tryCatch(
        get(sym, env),
        error = function(e)
          c()
      )
    #Don't return matrix code if length-1 vector (i.e, scalar)
    if(length(val) && !is.function(val)){
       if(pred(val)){
          val <- fun(val, ...)
       }
       return(val)
    }
  })
  out[!sapply(out, is.null)]
}

#' Transform an R Expression into LaTeX Code
#'
#' This function converts unquoted R expressions into LaTeX
#' code. It uses `extract values` to substitute for each symbol
#' in the expression its value in `env`. Then it deparses the
#' expression and inserts LaTeX code in the appropriate places,
#' including handling matrices and vectors.
#' @param eqn An R expression, optionally containing matrices.
#' @param env Environment in which to evaluate the expression.
#' Defaults to the caller environment of `equation2latex`.
#' @param emBiggen Logical determining whether to apply the
#' LaTeX Bigg modifier to parentheses in the output, which looks
#' better with matrices. "Embiggens" is a perfectly cromulent word.
#' Defaults to TRUE.
#'
#' @return LaTeX code for rendering the expression, as a length-one
#' character vector.
#' @export
#'
#' @examples
#'
#' A <- matrix(c(1, 2, 3, 4), nrow = 2)
#' B <- A + diag(x = 3, nrow = 2)
#' C <- matrix(c(7, 8, 9, 10), nrow = 2)
#' D <- matrix(c(1, 7, 9,-9), nrow = 2)
#' k <- 7
#'
#' print_eqn(equation2latex(A))
#' print_eqn(equation2latex(k *A + (B + C) %*% D))
#'
#'
equation2latex <- function(eqn, env, emBiggen = TRUE) {

  # For the nerds: why am I not using a default value here?
  if(missing(env)){
    env <- parent.frame()
  }
  pred <- function(val) length(val) > 1L || "matrix" %in% class(val)
  sub_matrix <- function(val) paste(mat2latex(val, sink = TRUE, slash_repeats = 2), collapse = "\n")

  eqn <- substitute(eqn)
  subs <- extract_values(eqn, env = env, pred = pred, fun = sub_matrix)
  #Replace each symbol in equation with LaTeX code for corresponding value
  out <-
    Reduce(function(x, y)
      gsub(
        pattern = y,
        replacement = subs[[y]],
        x = x
      ),
      names(subs),
      init = deparse(eqn)) |>
  (function(x) gsub("\\%|\\*", "", x))() #remove unneeded operands
  if (emBiggen) {
    out <- gsub("([()])", "\\\\Bigg \\1", out, perl = TRUE)
  }
  out
}
