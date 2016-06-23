#' Differential Mutation operators
#'
#' Implementation of the Differential Evolution variation operators for the
#' MOEA/D
#'
#' This function performs the variation operators of the DE algorithm. For
#' each candidate solution \eqn{X[i, ]}, a candidate vector \eqn{V[i, ]}
#' is generated according to:
#'
#'\code{V [i, ] = X [i, ]}
#'
#'\code{if runif(1) < CR, V [i, j] = X [i, j] + F * (X [r1, j] + X [r2, j]),
#'      with j = 1, ..., n}
#'
#' wherein \code{r1 != r2 != i} are mutually exclusive, randomly
#' selected indices that indicate vectors from either the neighborhood of
#' \code{i} (with probability \code{delta}) or the whole population \code{X}
#' (with probability \code{1 - delta})
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
#'        \item \code{chngpars$DE_diffmut$F} : scaling factor (REQUIRED).
#'        \item \code{chngpars$DE_diffmut$CR} : recombination probability
#'            (OPTIONAL. Defaults to 1).
#'        \item \code{chngpars$DE_diffmut$delta} : probability of selecting the
#'            'parent' vectors from the neighborhood of each candidate solution.
#'            (OPTIONAL. Defaults to 0.9).
#'    }
#' }
#'
#' @section References:
#' K. Price, R.M. Storn, J.A. Lampinen, "Differential Evolution: A
#' Practical Approach to Global Optimization", Springer 2005.\cr
#'
#' H. Li, Q. Zhang, "Multiobjective Optimization Problems With Complicated
#' Pareto Sets, MOEA/D and NSGA-II", IEEE. Trans. Evol. Comp. 12(2):284-302,
#' 2009.
#'
#' @return Matrix \code{U} containing the resulting population
#'
DEops <- function(){

  # Get access to the variables in the calling environment
  env   <- parent.frame()

  # ========== Error catching and default value definitions
  tmp <- assert_that(
    all(has_name(env, c("X", "N", "chngpars"))),
    has_name(env$chngpars, "DEops"),
    has_name(env$chngpars$DEops, "F"),
    identical(nrow(env$X), nrow(env$N)))

  if(!any("CR"  == names(env$chngpars$DEops)))   env$chngpars$DEops$CR    <- 1
  if(!any("delta" == names(env$chngpars$DEops))) env$chngpars$DEops$delta <- 0.9

  # ==========

  # Draw 'donor' vectors
  # 1) Using only the neighborhood
  R <- t(apply(env$N[, -1],
               MARGIN = 1,
               FUN = function(x) x[sample.int(n      = length(x),
                                              size   = 2,
                                              replace = FALSE)]))

  # 2) Reshuffle cases that will use the whole population
  useWhole      <<- which(runif(nrow(env$X)) > env$chngpars$DEops$delta)
  R[useWhole, ] <- t(vapply(useWhole,
                            FUN.VALUE = integer(2),
                            FUN = function(i,l) sample(c(1:l)[-i],
                                                       size    = 2,
                                                       replace = FALSE),
                            l = nrow(env$X)))

  # 3) Perform first DE operation
  V <- env$X + env$chngpars$DEops$F * (env$X[R[, 1], ] - env$X[R[, 2], ])

  # 4) Perform second DE operation
  if (env$chngpars$DEops$CR < 1){
    Mut <- randM(V) < env$chngpars$DEops$CR
    V <- V * Mut + env$X * (!Mut)
  }

  # Truncate to limits
  return(pmax(0*V, pmin(0*V + 1, V)))
}
