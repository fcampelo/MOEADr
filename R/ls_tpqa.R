#' Three-point quadratic approximation local search
#'
#' Three-point quadratic approximation (TPQA) local search implementation for
#' the MOEA/D
#'
#' This routine implements the 3-point quadratic approximation local search for
#' the MOEADr package. Check the references for details.
#'
#' This routine is intended to be used internally by [variation_localsearch()],
#' and should not be called directly by the user.
#'
#' @param Xt Matrix of incumbent solutions
#' @param Yt Matrix of objective function values for Xt
#' @param W matrix of weights (generated by [generate_weights()]).
#' @param B Neighborhood matrix, generated by [define_neighborhood()].
#' @param Vt List object containing information about the constraint violations
#' of the _incumbent solutions_, generated by [evaluate_population()]
#' @param scaling list containing the scaling parameters (see [moead()] for
#' details).
#' @param aggfun List containing the aggregation function parameters. See
#' Section `Scalar Aggregation Functions` of the [moead()] documentation for
#' details.
#' @param constraint list containing the parameters defining the constraint
#' handling method. See Section `Constraint Handling` of the [moead()]
#' documentation for details.
#' @param epsilon threshold for using the quadratic approximation value
#' @param which.x logical vector indicating which subproblems should undergo
#' local search
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @section References:
#' Y. Tan, Y. Jiao, H. Li, X. Wang,
#' "A modification to MOEA/D-DE for multiobjective optimization problems with
#' complicated Pareto sets",
#' Information Sciences 213(1):14-38, 2012.\cr
#'
#' Y.-C. Jiao, C. Dang, Y. Leung, Y. Hao,
#' "A modification to the new version of the prices algorithm for continuous
#' global optimization problems",
#' J. Global Optimization 36(4):609-626, 2006.\cr
#'
#' F. Campelo, L.S. Batista, C. Aranha (2020): The {MOEADr} Package: A
#' Component-Based Framework for Multiobjective Evolutionary Algorithms Based on
#' Decomposition. Journal of Statistical Software \doi{10.18637/jss.v092.i06}\cr
#'
#'
#' @return Matrix \code{X}' containing the modified population
#'
#' @export

ls_tpqa <- function(Xt, Yt, W, B, Vt, scaling, aggfun,
                    constraint, epsilon = 1e-6, which.x, ...){

  # ========== Error catching and default value definitions
  # All error catching and default value definitions are assumed to have been
  # verified in the calling function perform_variation().
  assertthat::assert_that(is.numeric(epsilon), epsilon > 0)
  if(ncol(B) < 3) stop("TPQA local search only works for neighbors$T > 3")
  # ==========

  # Objective scaling
  tpqa.normYs <- scale_objectives(Y       = Yt, # <--- this is intentional
                                  Yt      = Yt, # <--- this too
                                  scaling = scaling)

  # Scalarization by neighborhood.
  tpqa.bigZ <- scalarize_values(normYs  = tpqa.normYs,
                                W       = W,
                                B       = B,
                                aggfun  = aggfun)

  # Calculate performance indices for each subproblem
  tpqa.selind <- order_neighborhood(bigZ       = tpqa.bigZ,
                                    B          = B,
                                    V          = Vt, # <--- this is intentional
                                    Vt         = Vt, # <--- this too
                                    constraint = constraint)

  # Remove index "nrow(tpqa.bigZ)" (would indicate the "incumbent" solution in
  # the general use of the routines above) and return indices to the 3 best
  # solutions for each subproblem
  sels <- t(apply(tpqa.selind,
                  MARGIN = 1,
                  FUN    = function(x, ii) {
                    x <- x[x != ii]
                    return(x[1:3])},
                  ii     = nrow(tpqa.bigZ)))

  # Calculate matrix of q_{ij} coefficients
  Q <- (Xt[sels[, 2], , drop = FALSE] - Xt[sels[, 3], , drop = FALSE]) +
       (Xt[sels[, 3], , drop = FALSE] - Xt[sels[, 1], , drop = FALSE]) * 2 +
       (Xt[sels[, 1], , drop = FALSE] - Xt[sels[, 2], , drop = FALSE]) * 3

  # Calculate matrix of \hat{x}_{ij} values
  Xhat <- ((Xt[sels[, 2], , drop = FALSE] ^ 2 - Xt[sels[, 3], , drop = FALSE] ^ 2) +
           (Xt[sels[, 3], , drop = FALSE] ^ 2 - Xt[sels[, 1], , drop = FALSE] ^ 2) * 2 +
           (Xt[sels[, 1], , drop = FALSE] ^ 2 - Xt[sels[, 2], , drop = FALSE] ^ 2) * 3) / (2 * Q + 1e-16)


  # Calculate output matrix
  Xls             <- Xt[sels[, 1], , drop = FALSE] * (Q < epsilon) + Xhat * (Q >= epsilon)
  Xls[!which.x, ] <- NA

  # Return results
  return(Xls)
}
