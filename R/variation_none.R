#' Identity operator
#'
#' Identity operator (no variation performed)
#'
#' Performs the identity operator (no variation). This routine is included to
#' simplify the use of automated tuning / design tools such as Iterated Racing.
#'
#' @param X Population matrix
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @return Input matrix `X`
#'
#' @export

variation_none <- function(X, ...){
  return(X)
}
