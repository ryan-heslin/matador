`%^%` <- function(m, pow){

  replicate(n = pow, m, simplify = FALSE) %>%
    purrr::reduce(`%*%`)
}
