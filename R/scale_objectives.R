#' Scaling of the objective function values
#'
#' Performs scaling of the objective function values for the MOEADr package
#'
#' This routine scales the matrices of objective function values for the
#' current (\code{Yt}) and candidate (\code{Y}) solutions. The
#' following methods are currently available:
#'
#' \itemize{
#'    \item \code{scaling$name = "none"}: no scaling
#'    \item \code{scaling$name = "simple"}: simple linear scaling between
#'    estimated ideal and nadir points, calculated from the available points in
#'    \code{Y} and \code{Yt} at each iteration.
#' }
#'
#' @param Y matrix of objective function values for the incumbent solutions
#' @param Yt matrix of objective function values for the candidate solutions
#' @param scaling list containing the scaling parameters (see [moead()] for
#' details).
#' @param eps tolerance value for avoiding divisions by zero.
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @return List object containing scaled objective function value matrices
#' `Y` and `Yt`, as well as estimates of the "ideal" point `minP`` and "nadir"
#' point `maxP`.
#'
#' @export

scale_objectives <- function(Y, Yt, scaling, eps = 1e-16, ...){

  # ========== Error catching and default value definitions
  assertthat::assert_that(
    "name" %in% names(scaling),
    is.matrix(Y) & is.matrix(Yt),
    identical(dim(Y), dim(Yt)))

  # Get "ideal" and "nadir" points
  minP <- getminP(rbind(Y, Yt))
  maxP <- getmaxP(rbind(Y, Yt))

  # No scaling
  if (scaling$name == "none"){
    return(list(Y    = Y,
                Yt   = Yt,
                minP = minP,
                maxP = maxP))
  }

  # Simple scaling
  if (scaling$name == "simple"){
    # Replicate minP and maxP to matrix format (for dimensional consistency)
    MinP <- matrix(rep(minP, times = nrow(Y)),
                   nrow  = nrow(Y),
                   byrow = TRUE)
    MaxP <- matrix(rep(maxP, times = nrow(Y)),
                   nrow  = nrow(Y),
                   byrow = TRUE)

    # Perform linear scaling
    Y    <- (Y - MinP) / (MaxP - MinP + eps)
    Yt   <- (Yt - MinP) / (MaxP - MinP + eps)

    # In this case, minP = 0 and maxP = 1 by definition, so:
    minP <- 0 * minP
    maxP <- minP + 1

    return(list(Y    = Y,
                Yt   = Yt,
                minP = minP,
                maxP = maxP))
  }
}
