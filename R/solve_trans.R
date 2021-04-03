#' Compute a Linear Transformation on Arbitrary Vectors Using Known Inputs and Outputs
#' @description The solution is calculated using the generalized inverse, so the inputs matrix need not be square.
#' @param xes A matrix representing inputs to the transformation, of arbitrary rows and at least two columns.
#' @param txes A matrix representing outputs of the transformation, of arbitrary rows and the same number of columns of @param xes.
#' @param to_solve A matrix or numeric vector containing vectors on which the transformation will be computed, of the same number of rows
#' as @param xes and arbitrary columns.
#'
#' @return A matrix representing the transformation applied to each vector of @param to_solve, with as many rows as @param t_xes and as many columns
#' as @to_solve
#' @export
#'
#' @examples
#' # Example problems sourced from
#' #https://yutsumura.com/linear-algebra/linear-transformation-from-rn-to-rm/.
#' # Find matrix of transformation
#' solve_trans(matrix(c(1, 2, 3, 5), nrow =2 ), matrix(c(-3, 5, 7, 1), nrow = 2))
#'
#' # Solve for a particular vector
#' solve_trans(
#' xes = matrix(c(0, 1, 0, 0, 1, 1), nrow = 3),
#' txes = matrix(c(1, 2, 0, 1), nrow = 2),
#' to_solve = matrix(c(0, 1, 2)))
solve_trans <-
  function(xes, txes, to_solve = diag(nrow = nrow(xes))) {
    if (!is.matrix(to_solve)) {
      to_solve <- as.matrix(to_solve)
    }
    if (any(sapply(list(xes, txes), ncol) < 2)) {
      stop("Cannot compute transformation with fewer than 2 vectors")
    } else if (ncol(xes) != ncol(txes)) {
      stop("Cannot compute transformation; different numbers of inputs and outputs")
    } else if (nrow(xes) != nrow(to_solve)) {
      stop("Cannot solve transformation; diemnsions of inputs and vectors to solve for disagree")
    } else if (!all(sapply(list(xes, txes, to_solve), mode) == "numeric")) {
      stop("Cannot solve transformation; at least one non-numeric argument")
    }

    # Solve nonsquare by padding with zero vectors?
    out <- apply(to_solve, MARGIN = 2, function(x)
      gen_inverse(xes, x)) %>%
      t() %>% apply(., MARGIN = 1, function(x)
        txes %*% x)

    out
  }
