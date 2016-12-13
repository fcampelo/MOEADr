#' Print progress of MOEA/D
#'
#' Echoes progress of MOEA/D to the terminal for the MOEADr package
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
#'                               Defaults to \code{10}.
#'    }
#'    \item \code{iter} : counter that registers the iteration number
#' }
#'
#' @export

print_progress <- function(){

  # Get access to the variables in the calling environment
  moead.env  <- parent.frame()

  # ========== Error catching and default value definitions
  assertthat::assert_that(assertthat::has_name(moead.env, "showpars"))
  pars <- moead.env$showpars

  if(!any("show.iters" == names(pars))) pars$show.iters <- "numbers"
  if(!any("showevery" == names(pars)))  pars$showevery  <- 10

  assertthat::assert_that(
    assertthat::is.count(pars$showevery),
    any(pars$show.iters == c("dots", "numbers", "none")),
    length(pars$show.iters) == 1,
    length(pars$showevery) == 1)
  # ==========

  if (pars$show.iters != "none"){
    if (moead.env$iter == 1) cat("\nMOEA/D running: ")
    if (moead.env$iter %% pars$showevery == 0){
      if (pars$show.iters == "dots") cat(".")
      if (pars$show.iters == "numbers") cat("\nIteration: ", moead.env$iter)
    }
  }
}
