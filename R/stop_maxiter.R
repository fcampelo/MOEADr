#' Stop criterion: maximum number of iterations
#'
#' Verifies stop criterion "maximum number of iterations" for the MOEADr
#' package. For internal use only, not to be called directly by the user.
#'
#' When this stop criterion is used, one element of the \code{stopcrit}
#' parameter (see \code{\link{moead}}) must have the following structure:
#' \itemize{
#'    \item \code{stopcrit$name = "maxiter"}
#'    \item \code{stopcrit$maxiter = K}, where \code{K} is a positive integer.
#' }
#'
#' @param moead.env list representing the environment of the base function
#' \code{moead}.
#'
#' @return boolean value: TRUE if this criterion has been met, FALSE otherwise.
#'
#' @export
stop_maxiter <- function(moead.env){
  maxiter <- max(unlist(lapply(moead.env$stopcrit,
                               FUN = function(x){ifelse(x$name == "maxiter",
                                                        yes = x$maxiter,
                                                        no  = -Inf)})))
  assertthat::assert_that(assertthat::is.count(maxiter))

  return(moead.env$iter >= maxiter)
}
