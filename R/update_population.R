#' Update population
#'
#' Selection and population update procedures for the MOEA/D
#'
#' @section Parameters:
#' This routine accesses the required variables in the calling environment using
#' \code{parent.frame()}, so it does not require any explicit input parameters.
#' The calling environment must contain at least the following parameters:
#' \itemize{
#'    \item \code{Xt}: current population matrix
#'    \item \code{Yt}: matrix of objective values for population \code{Xt}
#'    \item \code{X} : candidate population matrix
#'    \item \code{Y} : matrix of objective values for population \code{X}
#'    \item \code{B} : neighborhood matrix (output of
#'    \code{define_neighborhood(...)$B})
#'    \item \code{W} : matrix of weigths (output of
#'    \code{generate_weights(...)})
#'    \item \code{aggfun}: list of scalarization parameters (see
#'        \code{\link{scalarize_values}()} for details).
#'    \item \code{scaling}: list of function scaling parameters (see
#'        \code{\link{scale_objectives}()} for details).
#'    \item \code{update}: list of update parameters, containing at least the
#'          field \code{update$name}, describing the method to be used for
#'          updating the population. Other parameters required for specific
#'          update methods are also included in this variable.
#'          The list of available update methods can be generated using
#'          \code{get_update_methods()}.
#' }
#'
#' @return The function does not explicitly return any value. However, it
#' modifies the state of the calling environment, updating the population
#' matrix \code{X} and the matrix of objective values \code{Y}.
#'
#' @export

update_population <- function(){

  # Get access to the variables in the calling environment
  moead.env   <- parent.frame()

  # ========== Error catching and default value definitions
  assertthat::assert_that(
    all(assertthat::has_name(moead.env, c("X", "Xt", "Y", "Yt", "B",
                                    "scaling", "update", "aggfun"))),
    nrow(moead.env$X) == nrow(moead.env$B),
    nrow(moead.env$X) == nrow(moead.env$Y),
    identical(dim(moead.env$X), dim(moead.env$Xt)),
    identical(dim(moead.env$Y), dim(moead.env$Yt)),
    identical(dim(moead.env$Y), dim(moead.env$W)))
  # ==========

  # Perform scaling and get updated estimate of the 'ideal' and 'nadir'
  # points
  normYs <- scale_objectives(moead.env)

  # Calculate matrix with scalarized performance values. Each column
  # contains the T scalarized performances of the candidate solutions in the
  # neighborhood of a given subproblem, plus the scalarized performance value
  # for the incumbent solution for that subproblem.
  bigZ <- scalarize_values(moead.env, normYs, moead.env$B)

  # copy bigZ to the main environment "moead()" (for use with variation
  # operators, if needed)
  moead.env$bigZ <- bigZ

  # ========== Generate vectors
  function_name <- paste0("updt_", tolower(moead.env$update$name))
  NextPop <- do.call(function_name,
                     args = list(moead.env = moead.env))

  moead.env$X <- NextPop$X
  moead.env$Y <- NextPop$Y

}
