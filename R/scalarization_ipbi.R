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
#' @param aggfun list containing parameters for the aggregation function. Must
#' contain the non-negative numeric constant `aggfun$theta`.
#' @param eps tolerance value for avoiding divisions by zero.
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @return Vector of scalarized performance values.
#'
#' @section References:
#' H. Sato,
#' "Inverted PBI in MOEA/D and its impact on the search performance on multi
#' and many-objective optimization."
#' Proceedings of the 2014 Annual Conference on Genetic and
#' Evolutionary Computation (GECCO), 2014.
#'
#' H. Sato,
#' "Analysis of inverted PBI and comparison with other scalarizing functions in
#' decomposition based MOEAs."
#' Journal of Heuristics 21(6):819-849, 2015
#'
#' @export

scalarization_ipbi <- function(Y, W, maxP, aggfun, eps = 1e-16, ...){

  # ========== Error catching and default value definitions
  assertthat::assert_that(
    is.matrix(Y) && is.matrix(W),
    identical(dim(W), dim(Y)),
    assertthat::has_name(aggfun, "theta"),
    length(maxP) == ncol(Y))
  # ==========

  # Replicate maxP (estimated nadir point) for dimensional consistency
  maxP <- matrix(maxP,
                 nrow  = nrow(Y),
                 ncol  = ncol(Y),
                 byrow = TRUE)

  # Norm of the weight vectors
  NormW <- matrix(sqrt(rowSums(W ^ 2)),
                  nrow  = nrow(W),
                  ncol  = ncol(W),
                  byrow = FALSE)

  # Sato's original version (Sato2014)
  # Calculate D1 (returns N x m numeric matrix with all columns equal, for
  # convenience in the calculation of D2).
  D1 <- matrix(abs(rowSums((maxP - Y) * W)) / NormW[, 1],
               nrow  = nrow(W),
               ncol  = ncol(W),
               byrow = FALSE)

  # Calculate D2 (returns numeric vector with N elements)
  D2 <- sqrt(rowSums(((maxP - Y) - D1 * W / NormW) ^ 2))


  # Lucas' version (to review)
  # D1 <- matrix(abs(rowSums((Y - maxP) * W)) / NormW[, 1],
  #              nrow = nrow(W),
  #              ncol = ncol(W),
  #              byrow = FALSE)
  # D2 <- sqrt(rowSums((Y - (maxP - D1 * W / NormW)) ^ 2))


  # Return G = -(d1 - theta*d2) = theta*d2 - d1
  # The multiplication by -1 is required since the iPBI is defined as
  # maximization instead of minimization.
  return(aggfun$theta * as.numeric(D2) - as.numeric(D1[, 1]))
}
