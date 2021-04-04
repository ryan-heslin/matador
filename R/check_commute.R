



#' Symbolically Represent Matrix Commutation
#'
#' @description This function generates symbolic matrices representing those
#' that might commute with a particular matrix M (i.e., `AM = MA`). This only
#' applies to square matrices; passing a nonsquare matrix will cause an error.
#' This function returns a matrix of the input matrix's dimension, with each
#' element consisting of an equation, where the left side is that element's
#' value multiplying on the left and the right its value multiplying on the
#' right. All matrices whose elements satisfy all the equations commute with
#' `M`.
#'
#' All n x n matrices trivially commute with the corresponding identity and
#' zero matrix, as well as the inverse, if it exists.
#' @param m An n x n matrix of dimension greater than 1 whose commutativity will
#' be tested.
#' @param syms Character vector of symbols to use to represent the commuting matrices.
#' Must have the same dimensions as @param m. Defaults to the first n^2 letters
#' of the alphabet, entered rowwise, as is conventional. If @param m has
#' dimension 6 or greater, letters are recycled as needed. If @param syms
#' includes digits and/or punctuation, an error results.
#' @param latex Logical. Return a LaTeX representation of the matrix using
#' `mat2latex`? Defaults to FALSE.
#' @param sink Argument passed on to `mat2latex`. Defaults to FALSE, ignored if
#' @param latex is false.
#'
#' @return An n x n matrix, where each element is an equation whose left-hand
#' side represents that elements value in `AM` and the right-hand side its value
#' in `MA`.
#' @export
#'
#' @examples check_commute(matrix(1:4, nrow = 2))
#' #Automatically drops zeroed terms
#' m <- matrix(sample(-1:1, 9, replace = TRUE), nrow = 3)
#' check_commute(m)
check_commute <-
  function(m,
           syms = matrix(rep_len(letters, length.out = prod(dim(m))),
                         nrow = nrow(m),
                         byrow = TRUE),
           latex = FALSE,
           sink = TRUE) {
    if (sum(dim(m)) < 4 |
        nrow(m) != ncol(m))
      stop("Commutation not possible for non-square matrix")
    if (length(syms) != prod(dim(m)))
      stop("Needed vector of", prod(dim(m)), "symbols, not", length(syms))
    if (any(grepl("[[:punct:]0-9]", x = syms)))
      stop("Cannot use punctuation or digits as symbols")

    LHS <-
      apply(syms, 1, function(x)
        apply(m, 2, function(y)
          clean_eqn(paste0(y, x)))) %>%
      t()

    RHS <-
      apply(m, 1, function(x)
        apply(syms, 2, function(y)
          clean_eqn(paste0(x, y)))) %>%
      t()

    out <- paste(LHS, RHS, sep = " = ") %>% matrix(nrow = nrow(m))

    if (latex) {
      out <- mat2latex(out, sink)
    }
    out

  }


#' Process Generated Equations for Readability
#'
#' @param eqn A character vector whose elements are terms in an algebraic
#' equation of the form `ax + by +cz...`, where `a`, `b`, and `c` are numeric
#' coefficients for unknown variables.
#'
#' @return a length-one character vector that drops zero-coefficient terms and
#' separates terms with the correct `+` and `-` signs. If all terms are zeroed,
#' the empty string.
#'
#'
#'
clean_eqn <- function(eqn) {
  out <-
    eqn %>% gsub("^0[^0-9.]+",
                 replacement = "",
                 x = .,
                 perl = TRUE) %>%
    gsub("(?<=^|-)1(?![0-9.])", "", x = ., perl = TRUE)
  out <- paste(out[out != ""], collapse = " + ") %>%
    gsub("\\+\\s-", "- ", x = ., perl = TRUE) %>%
    gsub("^$", "0", x = .) #empty means 0
  out

}
