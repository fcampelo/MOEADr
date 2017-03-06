#' Print progress of MOEA/D
#'
#' Echoes progress of MOEA/D to the terminal for the MOEADr package
#'
#' @param iter.times vector of iteration times of the [moead()] routine.
#' @param showpars list object containing parameters that control the printed
#'                 output of [moead()]. Parameter \code{showpars} can have the
#'                 following key-value pairs:
#'    \itemize{
#'      \item `$show.iters`: type of output ("dots", "numbers", or "none"). If
#'             not present in `showpars`, it defaults to "numbers";
#'      \item `$showevery`: positive integer that determines how frequently
#'             the routine echoes something to the terminal. If not present in
#'             `showpars`, it defaults to `10`.
#'    }
#'
#' @export

print_progress <- function(iter.times, showpars){

  if(!any("show.iters" == names(showpars))) showpars$show.iters <- "numbers"
  if(!any("showevery" == names(showpars)))  showpars$showevery  <- 10
  iter <- sum(iter.times != 0)

  assertthat::assert_that(
    assertthat::is.count(showpars$showevery),
    length(showpars$show.iters) == 1,
    showpars$show.iters %in% c("dots", "numbers", "none"),
    length(showpars$showevery) == 1)
  # ==========

  if (showpars$show.iters != "none"){
    if (iter == 1) cat("\nMOEA/D running: ")
    if (iter %% showpars$showevery == 0){
      if (showpars$show.iters == "dots") cat(".")
      if (showpars$show.iters == "numbers") cat("\nIteration:", iter,
                                                " | Elapsed time:",
                                                iter.times[iter],
                                                "seconds")
    }
  }
}
