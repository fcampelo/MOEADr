#' Weighted Sum Scalarization
#'
#' Perform Weighted Sum Scalarization for the MOEADr package.
#'
#' This routine calculates the scalarized performance values for the MOEA/D
#' using the  Weighted Sum method.
#'
#'
#' @param Y matrix of objective function values
#' @param W matrix of weights.
#' @param minP numeric vector containing estimated ideal point
#' @param eps tolerance value for avoiding divisions by zero.
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @return vector of scalarized performance values.
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

scalarization_ws <- function(Y, W, minP, eps = 1e-16, ...){

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

  Z <- apply(W * (Y - minP),
             MARGIN = 1,
             FUN    = sum)

  return(as.numeric(Z))

}
