#' Stop criterion: maximum runtime
#'
#' Verifies stop criterion "run time limit" for the MOEADr
#' package. For internal use only, not to be called directly by the user.
#'
#' When this stop criterion is used, one element of the \code{stopcrit}
#' parameter (see \code{\link{moead}}) must have the following structure:
#' \itemize{
#'    \item \code{stopcrit$name = "maxtime"}
#'    \item \code{stopcrit$maxtime = T}, where \code{T} is a positive integer
#'          representing the desired time limit (in seconds).
#' }
#'
#' @section Warning:
#' This function uses Sys.time() for verifying the total run time, i.e., it
#' counts wall-clock time, not CPU time.
#'
#' @param moead.env list representing the environment of the base function
#' \code{moead}.
#'
#' @return boolean value: TRUE if this criterion has been met, FALSE otherwise.
#'
#' @export
stop_maxtime <- function(moead.env){
  t.pars <- moead.env$stopcrit[[which(sapply(moead.env$stopcrit,
                                   function(x)x$name) == "maxtime")]]
  assertthat::assert_that(assertthat::is.count(t.pars$maxtime))

  cur.time <- Sys.time()
  if (moead.env$iter == 1){
    # perform initial setup
    moead.env$iter.times    <- numeric(10000)
    moead.env$iter.times[1] <- as.numeric(difftime(cur.time,
                                                   moead.env$time.start,
                                                   units = "secs"))

  } else{
    moead.env$iter.times[moead.env$iter] <- as.numeric(difftime(cur.time,
                                                                moead.env$cur.time,
                                                                units = "secs"))
  }
  moead.env$cur.time <- cur.time
  elapsed.time       <- sum(moead.env$iter.times[1:moead.env$iter])
  mean.iter.time     <- mean(moead.env$iter.times[1:moead.env$iter])

  # return TRUE if there is not enough time remaining for another iteration to
  # be performed
  return(elapsed.time + mean.iter.time >= t.pars$maxtime)
}
