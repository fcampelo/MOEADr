#' Polynomial mutation
#'
#' Polynomial mutation implementation for the MOEA/D
#'
#' This R implementation of the Polynomial Mutation reproduces the C code
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
#'    \item a list of variation parameters \code{chngpars}, containing:
#'    \itemize{
#'        \item \code{chngpars$polymut$eta} : mutation parameter (REQUIRED)
#'        \item \code{chngpars$polymut$pm}  : variable-wise probability of
#'                                            mutation (REQUIRED)
#'    }
#' }
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
#' @return Matrix \code{U} containing the recombined population
#'
polymut <- function(){

  # Get access to the variables in the calling environment
  env   <- parent.frame()

  # ========== Error catching and default value definitions
  tmp <- assert_that(
    all(has_name(env, c("X", "chngpars"))),
    has_name(env$chngpars, "polymut"),
    all(has_name(env$chngpars$polymut, c("eta", "pm"))))

  # ==========

  # Extract relevant variables from calling environment
  X     <- env$X
  pars  <- env$chngpars$polymut

  # Define positions that will be mutated
  R <- randM(X) <= pars$pm

  # Initialize offspring matrix
  V <- X * !R

  # Calculate Delta_q values (ATTENTION: RETURNING NAN FOR SOME REASON)
  Deltaq <- calc_Deltaq(X, pars$eta)

  # Update offspring
  V <- V + R * (X + Deltaq)

  # Truncate to limits and return
  return(pmax(0*V, pmin(0*V + 1, V)))
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
  Deltaq <- myflag * ((2*r + (1 - 2*r) * (1 - X) ^ (eta + 1)) ^ mexp - 1) +
    (!myflag) * (1 - (2*(1 - r) + 2*(r - 0.5) * X ^ (eta + 1)) ^ mexp)
  return (Deltaq)
}
