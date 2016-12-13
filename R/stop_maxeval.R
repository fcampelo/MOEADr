#' Stop criterion: maximum number of evaluations
#'
#' Verifies stop criterion "maximum number of evaluations" for the MOEADr
#' package. For internal use only, not to be called directly by the user.
#'
#' When this stop criterion is used, one element of the \code{stopcrit}
#' parameter (see \code{\link{moead}}) must have the following structure:
#' \itemize{
#'    \item \code{stopcrit$name = "maxeval"}
#'    \item \code{stopcrit$maxeval = K}, where \code{K} is a positive integer.
#' }
#'
#' @param moead.env list representing the environment of the base function
#' \code{moead}.
#'
#' @return boolean value: TRUE if this criterion has been met, FALSE otherwise.
#'
#' @export
stop_maxeval <- function(moead.env){
  maxeval <- max(unlist(lapply(moead.env$stopcrit,
                               FUN = function(x){ifelse(x$name == "maxeval",
                                                        yes = x$maxeval,
                                                        no  = -Inf)})))
  assertthat::assert_that(assertthat::is.count(maxeval))

  return(moead.env$nfe >= maxeval)
}
