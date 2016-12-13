#' Inverted Penalty-based Boundary Intersection Scalarization
#'
#' Perform inverted PBI Scalarization for the MOEADr package.
#'
#' This routine calculates the scalarized performance values for the MOEA/D
#' using the inverted PBI method.
#'
#'
#' @param Y matrix of objective function values
#' @param W matrix of weights.
#' @param maxP numeric vector containing estimated ideal point
#' @param aggf list containing parameters for the aggregation function (same as
#' the \code{aggfun} input variable of the \code{moead(...)} routine. Must
#' contain the non-negative numeric constant \code{aggf$theta}.
#' @param eps tolerance value for avoiding divisions by zero.
#' @param ... other parameters (unused, included for compatibility with
#' generic call)
#'
#' @return vector of scalarized performance values.
#'
#' @section References:
#' H. Sato,
#' "Inverted PBI in MOEA/D and its impact on the search performance on multi
#' and many-objective optimization."
#' Proceedings of the 2014 Annual Conference on Genetic and
#' Evolutionary Computation (GECCO), 2014.
#'
#' @export

scalarization_ipbi <- function(Y, W, maxP, aggf, eps = 1e-16, ...){

  # ========== Error catching and default value definitions
  assertthat::assert_that(
    is.matrix(Y) && is.matrix(W),
    identical(dim(W), dim(Y)),
    assertthat::has_name(aggf, "theta"),
    length(maxP) == ncol(Y))
  # ==========

  # Replicate maxP for dimensional consistency
  maxP <- matrix(maxP,
                 nrow = nrow(Y),
                 ncol = ncol(Y),
                 byrow = TRUE)

  # Norm of the weight vectors
  NormW <- matrix(sqrt(rowSums(W ^ 2)),
                  nrow  = nrow(W),
                  ncol  = ncol(W),
                  byrow = FALSE)

  # Calculate D1 and D2
  D1 <- matrix(rowSums((maxP - Y - eps) * W / NormW),
               nrow = nrow(W),
               ncol = ncol(W),
               byrow = FALSE)

  D2 <- sqrt(rowSums((maxP - Y - D1 * W) ^ 2))

  return(aggf$theta * D2 - as.numeric(D1[, 1]))

}
