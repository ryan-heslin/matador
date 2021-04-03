


#' Symbolically Represent Matrix Commutation
#'
#' @description This function generates symbolic matrices representing those
#' that might commute with a particular matrix M (i.e., `AM = MA`). This only
#' applies to square matrices; passing a nonsquare matrix will cause an error.
#' This function returns a matrix of the input matrix's dimension, with each
#' element consisting of an equation, where the left side is that element's
#' value multiplying on the left and the right its value multiplying on the
#' right. All matrices whose elements satisfy all the equations commute.
#'
#' All n x n matrices trivially commute with `the corresponding identity and
#' zero matrix, as well as the inverse, if it exists.
#' @param m An n x n matrix of dimension greater than 1 whose commutativity will
#' be tested.
#' @param syms Matrix of symbols to use to represent the commuting matrices.
#' Must have the same dimensions as @param m. Defaults to the first n^2 letters
#' of the alphabet, entered rowwise, as is conventional. If @param m has
#' dimension 6 or greater, letters are recycled as needed.
#' @param latex Logical. Return a LaTeX representation of the matrix using
#' `mat2latex`? Defaults to FALSE.
#' @param sink Argument passed on to `mat2latex`. Defaults to FALSE, ignored if
#' @param latex is false.
#'
#' @return
#' @export
#'
#' @examples check_commute(matrix(1:4, nrow = 2))
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

    LHS <-
      apply(syms, 1, function(x)
        apply(m, 2, function(y)
          paste(collapse = " + ", paste0(y, x)))) %>%
      t()

    RHS <-
      apply(m, 1, function(x)
        apply(syms, 2, function(y)
          paste(collapse = " + ", paste0(x, y)))) %>%
      t()

    out <- paste(LHS, RHS, sep = " = ") %>% matrix(nrow =nrow(m))

    if (latex) {
      out <- mat2latex(out, sink)
    }
    out

  }
