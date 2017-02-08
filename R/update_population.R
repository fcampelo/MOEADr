#' Update population
#'
#' Selection and population update procedures for the MOEA/D
#'
#' This update routine is intended to be used internally by the main [moead()]
#' function, and should not be called directly by the user. The list of
#' available update methods can be generated using [get_update_methods()].
#'
#' @param call.env calling environment (generated using, e.g., [environment()].
#'
#' @return List object containing the updated values of the population matrix
#' `X`, objective function matrix `Y`, and constraint values list `V`.
#'
#' @export

update_population <- function(call.env){

  # ========== Call specific update strategy
  function_name <- paste0("updt_", tolower(call.env$update$name))
  NextPop <- do.call(function_name,
                     args = list(call.env = call.env))

  # Return
  return(NextPop)
}
