#' Polynomial mutation
#'
#' Polynomial mutation implementation for the MOEA/D
#'
#' This R implementation of the Polynomial Mutation reproduces the C code
#' implementation available in the R package **emoa** 0.5-0, by Olaf Mersmann.
#' The differences between the present version and the original one are:
#' \itemize{
#'    \item The operator is performed on the variables scaled to the `[0, 1]`
#'    interval, which simplifies the calculations.
#'    \item Calculations are vectorized over variables, which also simplifies
#'          the implementation.
#' }
#'
#' @param X Population matrix
#' @param etam mutation constant
#' @param pm variable-wise probability of mutation
#' @param eps small constant used to prevent divisions by zero
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @section References:
#' K. Deb and S. Agrawal (1999). A Niched-Penalty Approach for Constraint
#' Handling in Genetic Algorithms. In: Artificial Neural Nets and Genetic
#' Algorithms, pp. 235-243, Springer.
#'
#' Olaf Mersmann (2012). emoa: Evolutionary Multiobjective
#' Optimization Algorithms. R package version 0.5-0.\cr
#' http://CRAN.R-project.org/package=emoa
#'
#' @return Matrix `X`' containing the mutated population
#'
#' @export

variation_polymut <- function(X, etam, pm, eps = 1e-6, ...){

  # ========== Error catching and default value definitions
  assertthat::assert_that(
    is.numeric(X) && is.matrix(X),
    is.numeric(etam) && etam > 0,
    is.numeric(pm) && is_within(pm, 0, 1, strict = FALSE))
  # ==========

  nflag <- FALSE
  if(!is_within(X, 0, 1, strict = FALSE)){
    # Standardize population matrix
    dimX <- dim(X)
    minP <- matrix(getminP(X),
                   nrow  = dimX[1],
                   ncol  = dimX[2],
                   byrow = TRUE)
    maxP <- matrix(getmaxP(X),
                   nrow  = dimX[1],
                   ncol  = dimX[2],
                   byrow = TRUE)
    X <- (X - minP) / (maxP - minP + eps)
    nflag <- TRUE
  }

  # Define positions that will be mutated
  R <- randM(X) <= pm

  # Calculate Delta_q values
  Deltaq <- calc_Deltaq(X, etam)

  # Update mutated population
  Xp <- X * (!R) + (X + Deltaq) * R

  # Return (de-standardized, if needed) results
  if (nflag){
    return(minP + Xp * (maxP - minP + eps))
  } else {
    return(Xp)
  }
}


# Aux. function: calculate Delta_q multiplier for Polynomial mutation
# Based on the C code available in the "emoa"
# package version 0.5-0, downloaded from Github (https://github.com/cran/emoa)
# (Commit SHA: 52609d900c114fcfe734afec736f39aec5ab34b2)
#
calc_Deltaq <- function(X, eta){
  r      <- randM(X)
  myflag <- (r <= 0.5)
  mexp   <- 1 / (eta + 1)
  Deltaq <- myflag * ((2 * r + (1 - 2 * r) * (1 - X) ^ (eta + 1)) ^ mexp - 1) +
    (!myflag) * (1 - (2 * (1 - r) + 2 * (r - 0.5) * X ^ (eta + 1)) ^ mexp)
  return (Deltaq)
}
