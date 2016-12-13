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
#' @section Parameters:
#' This routine receives a single input variable, \code{moead.env}, which is
#' generated within the calling function \code{update_population()}
#' (see \code{\link{update_population}} for more information).
#' This environment must contain at least the following variables:
#'
#' \itemize{
#'    \item \code{Yt}: matrix of objective values for population \code{Xt}
#'    \item \code{Y} : matrix of objective values for population \code{X}
#'    \item \code{scaling}: list of function scaling parameters, containing at
#'    least the field \code{scaling$name}.
#' }
#'
#' @param moead.env list representing the environment of the base function
#' \code{moead}.
#' @param eps tolerance value for avoiding divisions by zero.
#'
#' @return List object containing scaled objective function value matrices
#' \code{Y} and \code{Yt}, as well as "ideal" point \code{minP} and "nadir"
#' point \code{maxP}.
#'
#' @export

scale_objectives <- function(moead.env, eps = 1e-16){

  # ========== Error catching and default value definitions
  assertthat::assert_that(
    all(assertthat::has_name(moead.env, c("Y", "Yt", "scaling"))),
    assertthat::has_name(moead.env$scaling, "name"),
    is.matrix(moead.env$Y) & is.matrix(moead.env$Yt),
    ncol(moead.env$Y) == ncol(moead.env$Yt))


  # Get "ideal" and "nadir" points
  Y    <- moead.env$Y
  Yt   <- moead.env$Yt
  minP <- getminP(rbind(Y, Yt))
  maxP <- getmaxP(rbind(Y, Yt))

  # No scaling
  if (moead.env$scaling$name == "none"){
    return(list(Y    = Y,
                Yt   = Yt,
                minP = minP,
                maxP = maxP))
  }

  # Simple scaling
  if (moead.env$scaling$name == "simple"){
    # Replicate minP and maxP to matrix format (for dimensional consistency)
    MinP <- matrix(rep(minP, times = nrow(Y)),
                   nrow  = nrow(Y),
                   byrow = TRUE)
    MaxP <- matrix(rep(maxP, times = nrow(Y)),
                   nrow  = nrow(Y),
                   byrow = TRUE)

    # Perform linear scaling
    Y <- (Y - MinP) / (MaxP - MinP + eps)
    Yt <- (Yt - MinP) / (MaxP - MinP + eps)

    return(list(Y    = Y,
                Yt   = Yt,
                minP = minP,
                maxP = maxP))
  }
}
