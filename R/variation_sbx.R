#' Simulated binary crossover
#'
#' SBX implementation for the MOEA/D
#'
#' This R implementation of the Simulated Binary Crossover reproduces the C code
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
#' @param P Matrix of probabilities of selection for variation (created by
#' [define_neighborhood()]).
#' @param etax spread constant
#' @param pc variable-wise probability of recombination
#' @param eps smallest difference considered for recombination
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @section References:
#' Deb, K. and Agrawal, R. B. (1995) Simulated binary crossover for continuous
#' search space. Complex Systems, 9 115-148 \cr
#'
#' Olaf Mersmann (2012). emoa: Evolutionary Multiobjective
#' Optimization Algorithms. R package version 0.5-0.\cr
#' http://CRAN.R-project.org/package=emoa
#'
#' @return Matrix `X`' containing the recombined population
#'
#' @export

variation_sbx <- function(X, P, etax, pc = 1, eps = 1e-6, ...){

  # ========== Error catching and default value definitions
  assertthat::assert_that(
    is.numeric(X) && is.matrix(X),
    is.numeric(P) && is.matrix(P) && is_within(P, 0, 1, strict = FALSE),
    identical(nrow(X), nrow(P)),
    nrow(P) == ncol(P),
    is.numeric(etax) && etax > 0,
    is.numeric(pc) && is_within(pc, 0, 1, strict = FALSE),
    is.numeric(eps) && eps > 0)
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

  # Draw crossover pairs: for the i-th candidate solution, get two mutually
  # exclusive points according to the probabilities given in P[i, ].
  np <- nrow(X)
  Inds <- do.call(rbind,
                  lapply(1:np,
                         FUN = function(i, p){sample.int(n    = nrow(p),
                                                         size = 2,
                                                         prob = p[i, ])},
                         p   = P))

  # Initialize recombination matrices
  X1 <- X[Inds[, 1], , drop = FALSE]
  X2 <- X[Inds[, 2], , drop = FALSE]

  # Define positions that will be recombined
  R <- (randM(X) <= pc) & (abs(X1 - X2) > eps)

  # Initialize recombined solutions
  Xp1 <- X1 * !R
  Xp2 <- X2 * !R

  # Get ordered values
  U1 <- pmin(X1, X2)
  U2 <- pmax(X1, X2)
  Ur <- U2 - U1
  Ur[Ur < eps] <- eps # protection against divisions by zero.

  # Get Beta_q values
  Betaq1 <- calc_Betaq(1 + 2 * U1 / Ur, etax)
  Betaq2 <- calc_Betaq(1 + 2 * (1 - U2) / Ur, etax)

  # Generate offspring
  Xp1 <- Xp1 + 0.5 * R * (X1 + X2 - Betaq1 * Ur)
  Xp2 <- Xp2 + 0.5 * R * (X1 + X2 + Betaq2 * Ur)

  # Indicator matrix to randomly return v1 or v2 in each row.
  S <- matrix(stats::runif(nrow(X)) <= 0.5,
              nrow  = nrow(X),
              ncol  = ncol(X),
              byrow = FALSE)

  # Update recombined population
  Xp <- Xp1 * S + Xp2 * !S

  # Return (de-standardized, if needed) results
  if (nflag){
    return(minP + Xp * (maxP - minP + eps))
  } else {
    return(Xp)
  }
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

