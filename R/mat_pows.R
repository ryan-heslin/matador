#' Compute Powers of a Matrix
#'
#' @param m A square (n x n) numeric matrix
#' @param pows Numeric vector of distinct powers to raise m to. Must be positive natural numbers.
#'
#' @return A list of equal length to pows, each entry of which corresponds to that power of m.
#' @export
#'
#' @examples
#' m1 <- diag(x=3, nrow =4)
#' m2 <- matrix(c(1, 5, 5, 25), nrow =2) /26
#' mat_pows(m1, 1:5)
#' #Projection matrices are idempotent:
#' mat_pows(m2, c(1, 5, 1000))
mat_pows <- function(m, pows){

    if(any(pows <1 |pows %%1 !=0)){
      stop("Cannot compute non-integer or negative powers of matrices")
    } else if(mode(m) != "numeric"){
      stop("Cannot compute powers of non-numeric matrix")
    } else if(nrow(m) != ncol(m)){
      stop("Cannot compute powers of non-square matrix")
    }
    pows <- sort(unique(pows))
  purrr::map(pows, ~`%^%`(m, .x)) %>%
    stats::setNames(pows)
  }

