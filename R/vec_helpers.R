##' @name vec_helpers
##' @aliases l1
##' @aliases l2
##' @aliases normalize
##' @aliases angle
##' @aliases validate_vec
##'
##' @title Vector Helper Functions
##' @description These simple functions perform common computations on vectors:
##' `l1` and `l2` norms, unit length normalization, dot products, and angles.
##' @param v,v1,v2 Numeric vectors or 1 x `n` matrices. For the binary functions,
##' `v1` and `v2` must have the same length (if vectors) or number of rows (if
##' matrices).
##' @examples
##' dot(c(2, -1), c(1, 2))
##' angle(c(1, 0), c(0, 1))
##' normalize(rep(1, 16))
##' l1(-2:2)
##' l2(-2:2)
##' @rdname vec_helpers
##' @return `l1` returns the `l1` norm of the vector.
##' @export
l1 <- function(v) {
  validate_vec(v)
  sum(abs(v))
}
##'
##' @rdname vec_helpers
##' @return `l2` returns the `l2` norm of the vector.
##' @export
l2 <- function(v) {
  validate_vec(v)
  sqrt(sum(v ^ 2))
}
##'
##' @rdname vec_helpers
##' @return `normalize` returns the vector normalized to unit length, or
##' an error if passed the zero vector.
##' @export
normalize <- function(v) {
  validate_vec(v)
  if (l2(v) == 0)
    stop("Cannot normalize zero vector")
  v / l2(v)
}
##'
##' @rdname vec_helpers
##' @return `dot` returns the dot product of two vectors.
##' @export
dot <- function(v1, v2) {
  validate_vec(v1, v2)
  as.numeric((t(v1) %*% v2))

}
##'
##' @rdname vec_helpers
##' @return `angle` returns the angle between the two vectors in radians.
##' @export
#Compute angle between vectors
angle <- function(v1, v2) {
  acos(dot(v1, v2) / (l2(v1) * l2(v2)))
}

# Validator to check vector args
validate_vec <- function(...) {
  dots <- list(...)
  if (!all(sapply(dots, is.numeric)))
    stop("Vector must be numeric")
  if (!all(sapply(dots, function(x)
    is.null(dim(x)) || (dim(x)[2] == 1 & length(dim(
      x
    )) == 2)))) {
    stop("Vector must either be a vector or a matrix of arbitrary rows and one
         column")
  }
}
