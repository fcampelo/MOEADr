#' Three-point quadratic approximation local search
#'
#' Three-point quadratic approximation (TPQA) local search implementation for
#' the MOEA/D
#'
#' This routine implements the 3-point quadratic approximation local search for
#' the MOEADr package. Check the references for details.
#'
#' @param X Matrix of candidate solutions for local search
#' @param Y MAtrix of objective function values for X
#' @param B Matrix of neighborhoods for the subproblems in X
#' @param eps threshold for using the quadratic approximation value
#' @param ... other parameters (unused, included for compatibility with
#' generic call)
#'
#' @section References:
#' Y. Tan, Y. Jiao, H. Li, X. Wang,
#' "A modification to MOEA/D-DE for multiobjective optimization problems with
#' complicated Pareto sets",
#' Information Sciences 213(1):14-38, 2012.
#'
#' @return Matrix \code{X}' containing the modified population
#'
#' @export

ls_tpqa <- function(X, Y, B, eps = 1e-6, ...){

  ## PAREI AQUI


  # Return results
  return(X)
}
