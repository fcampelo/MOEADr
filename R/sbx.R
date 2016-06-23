#' Simulated binary crossover
#'
#' SBX implementation for the MOEA/D
#'
#' This R implementation of the Simulated Binary Crossover reproduces the C code
#' implementation available in package "emoa", version 0.5-0, by Olaf Mersmann.
#' The differences between the present version and the original one are:
#' \itemize{
#'    \item All variables are considered bounded in the (0, 1) interval, which
#'          simplifies some calculations.
#'    \item Calculations are vectorized over variables, which also simplifies
#'          some operations.
#' }
#'
#' @section Parameters:
#' This routine accesses all variables defined in the calling environment using
#' \code{parent.frame()}, so it does not require any explicit input parameters.
#' However, the calling environment must contain:
#' \itemize{
#'    \item a population matrix \code{X}
#'    \item a neighborhood matrix \code{N}
#'    \item a list of variation parameters \code{chngpars}, containing:
#'    \itemize{
#'        \item \code{chngpars$sbx$eta} : spread constant (REQUIRED)
#'        \item \code{chngpars$sbx$pc}  : variable-wise probability of crossover
#'                            (optional, defaults to 1)
#'        \item \code{chngpars$sbx$eps} : smallest difference considered by
#'                            crossover (optional, defaults to 1e-12)
#'    }
#' }
#'
#' @section References:
#' Deb, K. and Agrawal, R. B. (1995) Simulated binary crossover for continuous
#' search space. Complex Systems, 9 115-148 \cr
#'
#' Olaf Mersmann (2012). emoa: Evolutionary Multiobjective
#' Optimization Algorithms. R package version 0.5-0.\cr
#' http://CRAN.R-project.org/package=emoa
#'
#' @return Matrix \code{U} containing the recombined population
#'
sbx <- function(){

  # Get access to the variables in the calling environment
  env   <- parent.frame()

  # ========== Error catching and default value definitions
  tmp <- assert_that(
    all(has_name(env, c("X", "N", "chngpars"))),
    has_name(env$chngpars, "sbx"),
    has_name(env$chngpars$sbx, "eta"),
    identical(nrow(env$X), nrow(env$N)))

  X     <- env$X
  N     <- env$N
  pars  <- env$chngpars$sbx
  if(!any("pc"  == names(pars))) pars$pc  <- 1
  if(!any("eps" == names(pars))) pars$eps <- 1e-14

  # ==========

  # Draw crossover pairs
  Inds <- apply(N[,-1],
                MARGIN = 1,
                FUN = function(x) x[sample.int(length(x), 1)])

  # Initialize recombination matrices
  X1 <- X
  X2 <- X[Inds, ]

  # Define positions that will be recombined
  R <- (randM(X) <= pars$pc) & (abs(X1 - X2) > pars$eps)

  # Initialize offspring matrices
  V1 <- X1 * !R
  V2 <- X2 * !R

  # Get ordered values
  U1 <- pmin(X1, X2)
  U2 <- pmax(X1, X2)
  Ur <- U2 - U1
  Ur[Ur==0] <- 1e-15     # <--- protection against divisions by zero.

  # Get Beta_q values
  Betaq1 <- calc_Betaq(1 + 2 * U1 / Ur, pars$eta)
  Betaq2 <- calc_Betaq(1 + 2 * (1 - U2) / Ur, pars$eta)

  # Generate offspring
  V1 <- V1 + 0.5 * R * (X1 + X2 - Betaq1 * Ur)
  V2 <- V2 + 0.5 * R * (X1 + X2 + Betaq2 * Ur)

  # Indicator matrix to randomly return V1 or V2.
  S <- matrix(runif(nrow(X)) <= 0.5,
              nrow = nrow(X),
              ncol = ncol(X),
              byrow = FALSE)

  return(V1 * S + V2 * !S)
}


# Aux function: calculate Beta_q multiplier for Simulated Binary Crossover
# This implementation reproduces the C code available in the "emoa"
# package version 0.5-0, downloaded from Github (https://github.com/cran/emoa)
# (Commit SHA: 52609d900c114fcfe734afec736f39aec5ab34b2)
#
# The only change is the ability to calculate multiple Beta_q values at once
# (based on the dimension of the input matrix Beta)
calc_Betaq <- function(Beta, Eta){
  r      <- randM(Beta)
  alpha  <- 2 - Beta ^ -(Eta + 1)
  myflag <- (r <= 1 / alpha)
  Betaq  <- ((r * alpha) ^ (1 / (Eta + 1))) * myflag +
    (2 - r * alpha) ^ -(1 / (Eta + 1)) * (!myflag)
  return (Betaq)
}
