#' Truncate
#'
#' Truncation variation operator
#'
#' Truncate the solution matrix `X` to the `[0, 1]` interval.
#'
#' @param X Population matrix
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @return Truncated matrix `X`'.
#'
#' @export

variation_truncate <- function(X, ...){
  X <- matrix(pmax(0, pmin(X, 1)),
              nrow  = nrow(X),
              byrow = FALSE)
  return(X)
}
