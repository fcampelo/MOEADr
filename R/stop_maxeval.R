#' Stop criterion: maximum number of evaluations
#'
#' Verifies stop criterion "maximum number of evaluations" for the MOEADr
#' package. For internal use only, not to be called directly by the user.
#'
#' When this stop criterion is used, one element of the `stopcrit`
#' parameter (see [moead()]) must have the following structure:
#' \itemize{
#'    \item `stopcrit$name = "maxeval"`
#'    \item `stopcrit$maxeval`, containing a positive integer representing the
#'    desired maximum number of evaluations.
#' }
#'
#' @param stopcrit list containing the parameters defining the stop
#' handling method. See Section `Constraint Handling` of the [moead()]
#' documentation for details.
#' @param nfe evaluations counter of [moead()].
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @return boolean value: `TRUE` if this criterion has been met, `FALSE`
#' otherwise.
#'
#' @export
stop_maxeval <- function(stopcrit, nfe, ...){
  maxeval.i <- which(sapply(stopcrit,
                            function(x) x$name) == "maxeval")
  maxeval   <- stopcrit[[maxeval.i]]$maxeval
  assertthat::assert_that(assertthat::is.count(maxeval))

  return(nfe >= maxeval)
}
