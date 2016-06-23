#' Print progress of MOEA/D
#'
#' Echoes progress of MOEA/D to the terminal
#'
#' @section Parameters:
#' This routine accesses all variables defined in the calling environment using
#' \code{parent.frame()}, so it does not require any explicit input parameters.
#' However, the calling environment must contain:
#' \itemize{
#'    \item \code{showpars}: list containing parameters that control the printed
#'                           output of \code{moead()}. Parameter \code{showpars}
#'                           can have the following fields:
#'    \itemize{
#'      \item \code{$show.iters = c("dots", "numbers", "none")}: type of output.
#'                                Defaults to \code{"numbers"}.
#'      \item \code{$showevery}: positive integer that determines how frequently
#'                               the routine echoes something to the terminal.
#'                               Defaults to \code{1}.
#'    }
#'    \item \code{iters()} : counter function that registers the iteration
#'                           number
#' }
#'
#'
print_progress <- function(){

  # Get access to the variables in the calling environment
  env  <- parent.frame()

  # ========== Error catching and default value definitions
  tmp <- assert_that(has_name(env, "showpars"))
  pars <- env$showpars

  if(!any("show.iters" == names(pars))) pars$show.iters <- "numbers"
  if(!any("showevery" == names(pars)))  pars$showevery  <- 1
  if(!any("show.plot" == names(pars)))  pars$show.plot  <- FALSE

  tmp <- assert_that(
    is.count(pars$showevery),
    any(pars$show.iters == c("dots", "numbers", "none")),
    length(pars$show.iters) == 1,
    length(pars$showevery) == 1,
    is.flag(pars$show.plot))
  # ==========

  if (pars$show.iters != "none"){
    if (env$iters == 1) cat("\nMOEA/D running: ")
    if (env$iters %% pars$showevery == 0){
      if (pars$show.iters == "dots") cat(".")
      if (pars$show.iters == "numbers") cat("\nIteration: ", env$iters)
    }
  }

#   if (pars$show.plot &&
#       env$probpars$nobj == 2 &&
#       env$iters(0) %% pars$showevery == 0){
#     Y    <- env$Y
#     W    <- env$W
#     minP <- getminP(Y)
#     maxP <- getmaxP(Y)
#     xLim <- range(pretty(range(Y[, 1])))
#     yLim <- range(pretty(range(Y[, 2])))
#     plot(0, 0, type="n",
#          xlim = xLim, ylim = yLim,
#          xlab = "f1", ylab = "f2", main = "Objectives space",
#          las=1)
#     tmp<-apply(W,
#                MARGIN = 1,
#                function(w, minx, maxx){
#                  points(c(minx[1], minx[1] + (maxx[1] - minx[1]) * w[1]),
#                         c(minx[2], minx[2] + (maxx[2] - minx[2]) * w[2]),
#                         type = "l", lty = 1, col = rgb(0, 1, 0, alpha = 0.5))
#                },
#                minx = minP,
#                maxx = maxP)
#     points(Y[, 1], Y[, 2], type = "p",
#            pch = 20, col = 1)
#     Sys.sleep(0.05)
#   }
}
