#' Update population
#'
#' Selection and population update procedures for the MOEA/D
#'
#' This update routine is intended to be used internally by the main [moead()]
#' function, and should not be called directly by the user.
#'
#' @param call.env  calling environment (generated using, e.g., [environment()].
#' The calling environment must contain all parameters required by functions
#' [scalarize_values()], [scale_objectives()], and for the specific update
#' method given in `call.env$update`. The list of available update methods can
#' be generated using [get_update_methods()].
#'
#' @return List object containing the updated values of the population matrix
#' `X`, objective function matrix `Y`, and constraint values list `V`.
#'
#' @export

update_population <- function(call.env){

  # ========== Error catching and default value definitions
  #Y, Yt, scaling, B,

  assertthat::assert_that(
    all(assertthat::has_name(call.env, c("X", "Xt", "Y", "Yt",
                                         "V", "Vt", "B",
                                         "scaling", "update", "aggfun"))),
    identical(nrow(call.env$X), nrow(call.env$B)),
    identical(nrow(call.env$X), nrow(call.env$Y)),
    identical(dim(call.env$X),  dim(call.env$Xt)),
    identical(dim(call.env$Y),  dim(call.env$Yt)),
    identical(dim(call.env$V),  dim(call.env$Vt)),
    identical(dim(call.env$Y),  dim(call.env$W)))
  # ==========

  # Perform scaling and get updated estimate of the 'ideal' and 'nadir'
  # points
  normYs <- scale_objectives(Y       = call.env$Y,
                             Yt      = call.env$Yt,
                             scaling = call.env$scaling)

  # Calculate matrix with scalarized performance values. Each column
  # contains the T scalarized performances of the candidate solutions in the
  # neighborhood of a given subproblem, plus the scalarized performance value
  # for the incumbent solution for that subproblem.
  bigZ <- scalarize_values(call.env = call.env,
                           normYs   = normYs,
                           B        = call.env$B)


  # Calculate the index ordering matrix. Each row contains the indexes of the neighborhood,
  # in order of their "selection quality" (which takes into account both the performance value
  # and constraint handling policy, if any)
  moead.env$sel.indx <- order_neighborhood(moead.env)

  # ========== Call specific update strategy
  function_name <- paste0("updt_", tolower(moead.env$update$name))
  NextPop <- do.call(function_name,
                     args = list(moead.env = moead.env))

  # Return
  return(NextPop)
}
