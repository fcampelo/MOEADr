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
#' `X`, objective function matrix `Y`, and constraint values list `V`, as well
#' as an updated Archive list containing its corresponding components `X`, `Y`
#' and `V`.
#'
#' @export

update_population <- function(update, ...){

  # ========== Call specific update strategy
  function_name <- paste0("updt_", tolower(update$name))
  updt.args     <- as.list(sys.call())[-1]
  NextPop       <- do.call(function_name,
                           args = updt.args)

  # ========== Update Archive Population and Related Info
  if(updt.args$update$UseArchive){
    # Make sure that feasible solutions will always prevail in the Archive
    arch.args <- updt.args
    arch.args$constraint$name <- "vbr"
    arch.args$constraint$type <- "ts"

    # Make sure that the very best solution for each subproblem is always
    # attributed to it in the Archive, regardless of neighborhoods
    arch.args$update$name <- "best"
    arch.args$update$Tr   <- nrow(arch.args$X)
    arch.args$update$nr   <- nrow(arch.args$X)

    # Update Archive
    NextPop$Archive       <- do.call("updt_best",
                                     args = arch.args)
  } else NextPop$Archive <- NULL

  # ========== Return
  return(NextPop)
}
