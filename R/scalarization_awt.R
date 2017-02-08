#' Adjusted Weighted Tchebycheff Scalarization
#'
#' Perform Adjusted Weighted Tchebycheff Scalarization for the MOEADr package.
#'
#' This routine calculates the scalarized performance values for the MOEA/D
#' using the Adjusted Weighted Tchebycheff method.
#'
#'
#' @param Y matrix of objective function values
#' @param W matrix of weights.
#' @param minP numeric vector containing estimated ideal point
#' @param eps tolerance value for avoiding divisions by zero.
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @return Vector of scalarized performance values.
#'
#' @section References:
#' Y. Qi, X. Ma, F. Liu, L. Jiao, J. Sun, and J. Wu, “MOEA/D with
#' adaptive weight adjustment,” Evolutionary Computation, vol. 22,
#' no. 2, pp. 231–264, 2013.
#'
#' R. Wang, T. Zhang, and B. Guo, “An enhanced MOEA/D using uniform
#' directions and a pre-organization procedure,” in IEEE Congress on
#' Evolutionary Computation, Cancn, Mxico, 2013, pp. 2390–2397.
#'
#' @export

scalarization_awt <- function(Y, W, minP, eps = 1e-16, ...){

  # ========== Error catching and default value definitions
  assertthat::assert_that(
    is.matrix(Y) && is.matrix(W),
    identical(dim(W), dim(Y)),
    length(minP) == ncol(Y))
  # ==========

  # Replicate minP for dimensional consistency
  minP <- matrix(minP,
                 nrow  = nrow(Y),
                 ncol  = ncol(Y),
                 byrow = TRUE)

  # Calculating "normalized inverses"
  Rho <- (W + eps) ^ (-1) / rowSums((W + eps) ^ (-1))

  Z <- apply(Rho * (Y - minP + eps),
             MARGIN = 1,
             FUN    = max)

  return(as.numeric(Z))

}
