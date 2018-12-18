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
#' @section References:
#'
#' F. Campelo, L.S. Batista, C. Aranha (2020): The {MOEADr} Package: A
#' Component-Based Framework for Multiobjective Evolutionary Algorithms Based on
#' Decomposition. Journal of Statistical Software \doi{10.18637/jss.v092.i06}\cr
#'
#' @export

variation_truncate <- function(X, ...){
  X <- matrix(pmax(0, pmin(X, 1)),
              nrow  = nrow(X),
              byrow = FALSE)
  return(X)
}
