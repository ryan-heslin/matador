#Compute l2 norm
l1 <- function(v) {
  validate_vec(v)
  sum(abs(v))
}

#Compute l2 norm
l2 <- function(v) {
  validate_vec(v)
  sqrt(sum(v ^ 2))
}

# Normalize to unit vector
normalize <- function(v){
  validate_vec(v)
  v /l2(v)
}

# Compute dot product
dot <- function(v1, v2) {
  validate_vec(v1, v2)
  as.numeric((t(v1) %*% v2))

}

#Compute angle between vectors
angle <- function(v1, v2) {
  acos(dot(v1, v2) / (l2(v1) * l2(v2)))
}

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

#c(mode =  is.numeric(v), dims = is.null(dim(v)) || (dim(v)[2] == 1 & length(dim(v)) ==2)
