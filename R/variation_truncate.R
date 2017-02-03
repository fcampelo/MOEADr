#' Truncate
#'
#' Truncation variation operator
#'
#' Truncate the solution matrix X to guarantee that all values are in the
#' [0, 1] interval.
#'
#' @section References:
#' Code reused from https://github.com/fcampelo/ExpDE/blob/master/R/ExpDE.R
#' Lines 240-242
#'
#' @param X Population matrix
#' @param ... other parameters (unused, included for compatibility with
#' generic call)
#'
#' @return Matrix \code{X}
#'
#' @export

variation_truncate <- function(X, ...){
  X <- matrix(pmax(0, pmin(X, 1)),
              byrow = FALSE,
              nrow = nrow(X))
  return(X)
}
