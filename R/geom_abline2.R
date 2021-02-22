#' geom_abline2
#'
#'A wrapper to geom_abline intended to handle vertical lines (slope undefined). If geom_abline2 detects a slope argument of -Inf, it
#'it treats the intercept argument as an x-intercept, converts the call to a call to geom_vline, and evaluates it.
#'If no intercept is supplied, it uses geom_abline's default value of 0. Otherwise it evaluates
#'the call normally. Errors will result if the slope argument is Inf but is not named.
#' @param ... Arguments to geom_abline. Only modified if slope = Inf; otherwise passed on directly to geom_abline
#'
#' @return
#'
#'
#' @examples
geom_abline2 <- function(...) {

  call <- match.call()

  if(is.null(call$slope)){
    stop("Must name slope argument if using vertical slope mode")
  }
  #If intercept arg is infinte, treat as vertical slope and convert call to geom_vline
  if (!is.null(call$slope) & as.character(call$slope) == "Inf") {

    # Supply default if missing
    call$xintercept <- ifelse(is.null(call$intercept), 0, call$intercept)
    call$intercept <- NULL
    call$slope <- NULL
    call[[1]] <- quote(ggplot2::geom_vline)
    eval(call, envir = parent.frame())
  } else{
    ggplot2::geom_abline(...)
  }
}
