#' Stop criterion: maximum number of iterations
#'
#' Verifies stop criterion "maximum number of iterations" for the MOEADr
#' package. For internal use only, not to be called directly by the user.
#'
#' When this stop criterion is used, one element of the `stopcrit`
#' parameter (see [moead()]) must have the following structure:
#' \itemize{
#'    \item `stopcrit$name = "maxiter"`
#'    \item `stopcrit$maxiter`, containing a positive integer representing the
#'    desired maximum number of iterations.
#' }
#'
#' @param stopcrit list containing the parameters defining the stop
#' handling method. See Section `Constraint Handling` of the [moead()]
#' documentation for details.
#' @param iter iterations counter of [moead()].
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @return boolean value: `TRUE` if this criterion has been met, `FALSE`
#' otherwise.
#'
#' @export
stop_maxiter <- function(stopcrit, iter, ...){
  maxiter.i <- which(sapply(stopcrit,
                            function(x) x$name) == "maxiter")
  maxiter   <- stopcrit[[maxiter.i]]$maxiter
  assertthat::assert_that(assertthat::is.count(maxiter))

  return(iter >= maxiter)
}
