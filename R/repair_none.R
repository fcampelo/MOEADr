#' Identity operator
#'
#' Identity operator (no repair performed)
#'
#' Performs the identity operator (no repair). This routine is included to
#' simplify the use of automated tuning / design tools such as Iterated Racing.
#'
#' @param X Population matrix
#' @param ... other parameters (unused, included for compatibility with
#' generic call)
#'
#' @return Matrix \code{X}
#'
#' @export

repair_none <- function(X, ...){

  return(X)
}
