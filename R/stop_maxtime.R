#' Stop criterion: maximum runtime
#'
#' Verifies stop criterion "run time limit" for the MOEADr
#' package. For internal use only, not to be called directly by the user.
#'
#' When this stop criterion is used, one element of the `stopcrit`
#' parameter (see [moead()]) must have the following structure:
#' \itemize{
#'    \item `stopcrit$name = "maxtime"`
#'    \item `stopcrit$maxtime`, containing a positive integer representing the
#'    desired time limit (in seconds).
#' }
#'
#' @section Warning:
#' This function uses Sys.time() for verifying the total run time, i.e., it
#' counts wall-clock time, not CPU time.
#'
#' @param stopcrit list containing the parameters defining the stop
#' handling method. See Section `Constraint Handling` of the [moead()]
#' documentation for details.
#' @param iter.times vector containing the times spent by each iteration of the
#' moead() routine, up to the current one.
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @return boolean value: `TRUE` if this criterion has been met, `FALSE`
#' otherwise.
#'
#' @export
stop_maxtime <- function(stopcrit, iter.times, ...){
  t.pars <- stopcrit[[which(sapply(stopcrit,
                                   function(x)x$name) == "maxtime")]]
  assertthat::assert_that(is.numeric(t.pars$maxtime),
                          t.pars$maxtime > 0)

  elapsed.time       <- sum(iter.times)
  mean.iter.time     <- mean(iter.times)

  # return TRUE if there is not enough time remaining for another iteration to
  # be performed
  return(elapsed.time + mean.iter.time >= t.pars$maxtime)
}
