#' Polynomial mutation
#'
#' Polynomial mutation implementation for the MOEA/D
#'
#' This R implementation of the Polynomial Mutation reproduces the C code
#' implementation available in package "emoa", version 0.5-0, by Olaf Mersmann.
#' The differences between the present version and the original one are:
#' \itemize{
#'    \item All variables are considered bounded in the (0, 1) interval, which
#'          simplifies the calculations.
#'    \item Calculations are vectorized over variables, which also simplifies
#'          the operator.
#' }
#'
#' @param X Population matrix
#' @param etam mutation constant
#' @param pm variable-wise probability of mutation
#' @param ... other parameters (unused, included for compatibility with
#' generic call)
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
#' @return Matrix \code{X}' containing the mutated population
#'
#' @export

variation_polymut <- function(X, etam, pm, ...){

  # ========== Error catching and default value definitions
  assertthat::assert_that(
    is.numeric(X) && is.matrix(X),
    is.numeric(etam) && etam > 0,
    is.numeric(pm) && is_within(pm, 0, 1, strict = FALSE))
  # ==========


  # Define positions that will be mutated
  R <- randM(X) <= pm

  # Calculate Delta_q values
  Deltaq <- calc_Deltaq(X, etam)

  # Update mutated population
  V <- X * !R + R * (X + Deltaq)

  # Truncate to [0,1] and return
  return(pmax(0 * V, pmin(0 * V + 1, V)))
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
