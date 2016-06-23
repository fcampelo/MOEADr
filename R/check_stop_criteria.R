#' Stop criteria for MOEA/D
#'
#' Implements different stop criteria for the MOEA/D
#'
#' @section Parameters:
#' This routine accesses all variables defined in the calling environment using
#' \code{parent.frame()}, so it does not require any explicit input parameters.
#' However, the calling environment must contain:
#' \itemize{
#'    \item \code{iters()} : counter function that registers the iteration
#'                           number
#'    \item \code{nfe()} : counter function that registers the number of
#'                           candidate solutions evaluated.
#'    \item \code{stopcrit}: list defining the stop criteria for the MOEA/D.
#'                           This list can contain the following fields:
#'    \itemize{
#'      \item \code{$maxiter}: positive integer indicating the maximum
#'                              number of iterations allowed.
#'      \item \code{$maxeval}: positive integer indicating the maximum
#'                              number of candidate solutions evaluated.
#'      \item \code{$names = c("stop_maxiter", "stop_maxeval")}:
#'            character vector containing the names of the
#'            stop criteria to be used.
#'    }
#' }
#'
#' @return The function does not return any value. However, it modifies the
#' state of the calling environment, updating the iteration flag
#' \code{keep.running}.
#'
check_stop_criteria <- function(){

  # Get access to the variables in the calling environment
  env <- parent.frame()

  # ========== Error catching and default value definitions
  tmp <- assert_that(
    all(has_name(env, c("iters", "nfe", "stopcrit"))),
    has_name(env$stopcrit, "names"),
    all(env$stopcrit$names %in% c("stop_maxiter",
                                  "stop_maxeval")))
  # ==========

  crits <- env$stopcrit$names
  keep.running <- env$keep.running
  for (crit in crits){
    keep.running <- keep.running * !(do.call(crit,
                                             args = list()))
  }

  env$keep.running <- keep.running
}

# Stop criterion: maximum number of iterations
stop_maxiter <- function(){
  env <- parent.frame(n = 2)
  assert_that(
    has_name(env$stopcrit, "maxiter"),
    is.count(env$stopcrit$maxiter))

  return(env$iters >= env$stopcrit$maxiter)
}

stop_maxeval <- function(){
  env <- parent.frame(n = 2)
  assert_that(
    has_name(env$stopcrit, "maxeval"),
    is.count(env$stopcrit$maxeval))

  return(env$nfe >= env$stopcrit$maxeval)
}
