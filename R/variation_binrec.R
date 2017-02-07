#' Binomial Recombination
#'
#' Binomial recombination implementation for the MOEA/D.
#'
#' This variation operator only works if at least one other variation operator
#' is performed prior to its execution, otherwise it becomes an identity
#' operator (returns an unchanged matrix X).
#'
#' @param X Population matrix
#' @param rho mutation probability
#' @param Xt Original population matrix
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @return Matrix `X`' containing the recombined population
#'
#' @section References:
#' K. Price, R.M. Storn, J.A. Lampinen, "Differential Evolution: A
#' Practical Approach to Global Optimization", Springer 2005
#'
#' @export

variation_binrec <- function(X, Xt, rho, ...){

  # ========== Error catching and default value definitions
  assertthat::assert_that(
    is.numeric(X) && is.matrix(X),
    is.numeric(Xt) && is.matrix(Xt),
    identical(dim(X), dim(Xt)),
    is.numeric(rho) && is_within(rho, 0, 1, strict = FALSE))
  # ==========

  # Recombination matrix
  R <- randM(X) < rho

  # Guarantee that at least one variable is recombined for each candidate
  # solution
  K <- matrix(FALSE, nrow(X), ncol(X))
  for (i in 1:nrow(X)) {
    K[i, sample(ncol(X), size = 1)] <- TRUE
  }
  R <- R | (rowSums(R) == 0 & K)

  # Perform recombination and return
  return(R * X + (!R) * Xt)
}
