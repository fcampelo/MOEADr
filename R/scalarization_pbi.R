#' Penalty-based Boundary Intersection Scalarization
#'
#' Perform PBI Scalarization for the MOEADr package.
#'
#' This routine calculates the scalarized performance values for the MOEA/D
#' using the PBI method.
#'
#'
#' @param Y matrix of objective function values
#' @param W matrix of weights.
#' @param minP numeric vector containing estimated ideal point
#' @param aggfun list containing parameters for the aggregation function. Must
#' contain the non-negative numeric constant `aggfun$theta`.
#' @param eps tolerance value for avoiding divisions by zero.
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @return Vector of scalarized performance values.
#'
#' @section References:
#' Q. Zhang and H. Li, "MOEA/D: A Multiobjective Evolutionary Algorithm
#   Based on Decomposition", IEEE Trans. Evol. Comp. 11(6): 712-731, 2007.
#'
#' H. Li, Q. Zhang, "Multiobjective Optimization Problems With Complicated
#' Pareto Sets, MOEA/D and NSGA-II", IEEE. Trans. Evol. Comp. 12(2):284-302,
#' 2009.
#'
#' @export

scalarization_pbi <- function(Y, W, minP, aggfun, eps = 1e-16, ...){

  # ========== Error catching and default value definitions
  assertthat::assert_that(
    is.matrix(Y) && is.matrix(W),
    identical(dim(W), dim(Y)),
    assertthat::has_name(aggfun, "theta"),
    length(minP) == ncol(Y))
  # ==========

  # Replicate minP for dimensional consistency
  minP <- matrix(minP,
                 nrow  = nrow(Y),
                 ncol  = ncol(Y),
                 byrow = TRUE)

  # Norm of the weight vectors
  NormW <- matrix(sqrt(rowSums(W ^ 2)),
                  nrow  = nrow(W),
                  ncol  = ncol(W),
                  byrow = FALSE)

  # Calculate D1 and D2
  D1 <- matrix(rowSums((Y - minP + eps) * W) / NormW,
               nrow  = nrow(W),
               ncol  = ncol(W),
               byrow = FALSE)

  D2 <- sqrt(rowSums((Y - minP - D1 * W / NormW) ^ 2))

  return(as.numeric(D1[, 1] + aggfun$theta * D2))

}
