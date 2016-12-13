#' Stop criteria for MOEA/D
#'
#' Verifies stop criteria for the MOEADr package
#'
#' @section Parameters:
#' This routine accesses all variables defined in the calling environment using
#' \code{parent.frame()}, so it does not require any explicit input parameters.
#'
#' However, the calling environment must containat least a variable named
#' \code{stopcrit}, which is a list vector containing the stop criteria to be
#' used. Each element of this list vector must have at least a
#' parameter \code{stopcrit$name}, containing the name of the
#' criterion to be used, as well any other criterion-specific values
#' required. The list of available variation operators can be
#' generated using \code{get_stop_criteria()}. Other variables may also be
#' required (e.g., \code{iter} counter for iterations or \code{nfe} counter for
#' number of candidate solutions evaluated), depending on the criteria used.
#'
#' @return The function does not return any value. However, it modifies the
#' state of the calling environment, updating the iteration flag
#' \code{keep.running}.
#'
#' @export

check_stop_criteria <- function(){

  # Get access to the variables in the calling environment
  moead.env <- parent.frame()

  # ========== Error catching and default value definitions
  assertthat::assert_that(
    assertthat::has_name(moead.env, "stopcrit"),
    all(unlist(lapply(moead.env$stopcrit,
                      function(x){assertthat::has_name(x, "name")}))))
  # ==========

  crits <- unlist(lapply(moead.env$stopcrit,
                         function(x){x$name}))
  keep.running <- moead.env$keep.running
  for (crit in crits){
    function_name <- paste0("stop_", tolower(crit))
    keep.running <- keep.running * !(do.call(function_name,
                                             args = list(moead.env)))
  }

  moead.env$keep.running <- keep.running
}
