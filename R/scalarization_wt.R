#' Weighted Tchebycheff Scalarization
#'
#' Perform Weighted Tchebycheff Scalarization for the MOEADr package.
#'
#' This routine calculates the scalarized performance values for the MOEA/D
#' using the  Weighted Tchebycheff method.
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
#' Q. Zhang and H. Li, "MOEA/D: A Multiobjective Evolutionary Algorithm
#' Based on Decomposition", IEEE Trans. Evol. Comp. 11(6): 712-731, 2007.\cr
#'
#' H. Li, Q. Zhang, "Multiobjective Optimization Problems With Complicated
#' Pareto Sets, MOEA/D and NSGA-II", IEEE. Trans. Evol. Comp. 12(2):284-302,
#' 2009.\cr
#'
#' F. Campelo, L.S. Batista, C. Aranha (2020): The {MOEADr} Package: A
#' Component-Based Framework for Multiobjective Evolutionary Algorithms Based on
#' Decomposition. Journal of Statistical Software \doi{10.18637/jss.v092.i06}\cr
#'
#' @examples
#' W    <- generate_weights(decomp = list(name = "sld", H = 19), m = 2)
#' Y    <- matrix(runif(40), ncol = 2)
#' minP <- apply(Y, 2, min)
#' Z    <- scalarization_wt(Y, W, minP)
#'
#' @export

scalarization_wt <- function(Y, W, minP, eps = 1e-16, ...){

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

  Z <- apply(W * (Y - minP + eps),
             MARGIN = 1,
             FUN    = max)

  return(as.numeric(Z))

}
