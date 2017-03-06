#' Stop criteria for MOEA/D
#'
#' Verifies stop criteria for the MOEADr package.
#'
#' This routine is intended to be used internally by [moead()],
#' and should not be called directly by the user.
#'
#' @param stopcrit list containing the parameters defining the stop
#' handling method. See Section `Constraint Handling` of the [moead()]
#' documentation for details.
#' @param call.env List vector containing the stop criteria to be used.
#' See [moead()] for details.
#'
#' @return Flag `keep.running`, indicating whether the algorithm should continue
#' (`TRUE`) or terminate (`FALSE`).
#'
#' @export

check_stop_criteria <- function(stopcrit, call.env){

  # ========== Error catching and default value definitions
  assertthat::assert_that(
    all(unlist(lapply(call.env$stopcrit,
                      function(x){assertthat::has_name(x, "name")}))))
  # ==========

  crits <- unlist(lapply(stopcrit,
                         function(x){x$name}))

  keep.running <- TRUE

  # Check criteria
  for (i in seq_along(crits)){
    function_name <- paste0("stop_", tolower(crits[i]))
    keep.running  <- keep.running  & !do.call(function_name,
                                              args = as.list(call.env))
  }

  # Output
  return(keep.running)
}
