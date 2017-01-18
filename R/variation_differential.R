#' Differential Mutation
#'
#' Differential Mutation implementation for the MOEA/D
#'
#' This function generalizes many variations of the Differential Mutation
#' operator with general form:
#'
#' u <- x_basis + Phi(x_a - x_b)
#'
#' Where u is the new candidate vector, Phi is a real number != 0,
#' and x_basis, x_a and x_b are distinct vectors in the population.
#'
#' This function include the following variations of this operator:
#'
#' \itemize{
#'    \item Phi may be a constant, user provided parameter, or randomly choosen.
#'    \item X_basis can be a random vector from the population
#'    \item X_basis can be the mean point in the T neighborhood
#'    \item X_basis can be the weighted mean point in the T neighborhood
#' }
#'
#' @param X Population matrix
#' @param phi Mutation parameter, random if NULL
#' @param basis how to select the basis vector, can be one of "random", "mean" or "weighted"
#' @param ... other parameters (unused, included for compatibility with
#' generic call)
#'
#' @return Matrix \code{X}' containing the mutated population
#'
#' @export

variation_differential <- function(X, phi = NULL, basis = 'random', ...){

  # ========== Error catching and default value definitions
  assertthat::assert_that(
    is.numeric(X) && is.matrix(X),
    is.null(phi) || (is.numeric(phi) && phi != 0),
    is.element(basis,c('random', 'mean', 'weighted')))
  # ==========

  # Generate replacement indexes for xbasis, x0, x1
  # (Basis is recreated if 'mean' or 'random')
  R <- t(sapply(rep(nrow(X), nrow(X)), FUN = sample.int, size = 3, replace = FALSE))

  if (is.null(phi)) {
    phi <- runif(1)
  }

  Xn <- X[R[, 1], ] + phi(X[R[, 2], ] - X[R[, 3], ])

  return(Xn)
}


