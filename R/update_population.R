#' Update population
#'
#' Selection and population update procedures for the MOEA/D
#'
#' @section Parameters:
#' This routine accesses all variables defined in the calling environment using
#' \code{parent.frame()}, so it does not require any explicit input parameters.
#' However, the calling environment must contain:
#' \itemize{
#'    \item \code{Xt}: current population matrix
#'    \item \code{Yt}: matrix of objective values for population \code{Xt}
#'    \item \code{X} : candidate population matrix
#'    \item \code{Y} : matrix of objective values for population \code{X}
#'    \item \code{N} : neighborhood matrix
#'    \item \code{W} : matrix of weigths
#'    \item \code{scalpars}: list of scalarization parameters (see
#'        \code{scalarize_values} for details).
#'    \item \code{updtpars}: list of update parameters, containing:
#'    \itemize{
#'    \item \code{updtpars$method} : method to be used for update.
#'        (REQUIRED. Accepts "standard" (Zhang and Li, 2007) or "moead-de"
#'        (Li and Zhang, 2009).
#'        \item \code{updtpars$nr} : maximum number of candidate solutions
#'            replaced by each 'offspring' vector, when method = "moead-de".
#'            (REQUIRED if method = "moead-de", IGNORED if method = "standard").
#' }
#'
#' @section References:
#' Q. Zhang and H. Li, "MOEA/D: A Multiobjective Evolutionary Algorithm
#   Based on Decomposition", IEEE Trans. Evol. Comp. 11(6): 712-731, 2007.
#'
#' H. Li, Q. Zhang, "Multiobjective Optimization Problems With Complicated
#' Pareto Sets, MOEA/D and NSGA-II", IEEE. Trans. Evol. Comp. 12(2):284-302,
#' 2009.
#'
#' @return The function does not return any value. However, it modifies the
#' state of the calling environment, updating the population matrix \code{X} and
#' the matrix of objective values \code{Y}.
#'
update_population <- function(){

  # Get access to the variables in the calling environment
  env   <- parent.frame()

  # ========== Error catching and default value definitions
  assert_that(
    all(has_name(env, c("X", "Xt", "Y", "Yt",  "N", "updtpars", "scalpars"))),
    nrow(env$X) == nrow(env$N),
    nrow(env$X) == nrow(env$Y),
    identical(dim(env$X), dim(env$Xt)),
    identical(dim(env$Y), dim(env$Yt)),
    identical(dim(env$Y), dim(env$W)))

  assert_that(env$updtpars$method %in% c("standard",
                                         "moead-de"),
              length(env$updtpars$method)==1)

  if(has_name(env$scalpars, "normalize.obj")){
    assert_that(is.flag(env$scalpars$normalize.obj))
  } else {
    env$scalpars$normalize.obj <- FALSE
  }

  # ==========

  if (env$updtpars$method == "standard") {
    NextPop <- update_standard(env)
  }

  if (env$updtpars$method == "moead-de") {
    assert_that(has_name(env$updtpars, "nr"))
    NextPop <- update_moeadde(env)
  }

  env$X <- NextPop$X
  env$Y <- NextPop$Y

}
