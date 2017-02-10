#' Update population
#'
#' Selection and population update procedures for the MOEA/D
#'
#' This update routine is intended to be used internally by the main [moead()]
#' function, and should not be called directly by the user. The list of
#' available update methods can be generated using [get_update_methods()].
#'
#' @param update List containing the population update parameters. See
#' Section `Update Strategies` of the [moead()] documentation for
#' details.
#' @param ... other parameters to be passed down to the specific
#' `updt_`**xyz**`()` routines.
#'
#' @return List object containing the updated values of the population matrix
#' `X`, objective function matrix `Y`, and constraint values list `V`.
#'
#' @export

update_population <- function(update, ...){

  # ========== Call specific update strategy
  function_name <- paste0("updt_", tolower(update$name))
  NextPop <- do.call(function_name,
                     args = as.list(sys.call())[-1])

  # Return
  return(NextPop)
}
