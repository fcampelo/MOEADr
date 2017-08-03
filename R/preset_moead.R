#' preset_moead
#'
#' Standard MOEA/D Preset (Zhang and Li 2007)
#'
#' Returns a configuration object that replicates the
#' MOEA/D as in Zhang and Li (2007) (sec. V-E, p.721-722)
#'
#' @examples
#' configuration <- preset_moead(name = "original")
#' configuration$stopcrit = list(list(name = "maxiter", maxiter = 50))
#'
#' library(smoof)
#' ZDT1 <- make_vectorized_smoof(prob.name  = "ZDT1",
#'                               dimensions = 30)
#'                               problem   <- list(name       = "ZDT1",
#'                                                 xmin       = rep(0, 30),
#'                                                 xmax       = rep(1, 30),
#'                                                 m          = 2)
#' showpars  <- list(show.iters = "dots", showevery  = 10)
#' seed      <- 42
#'
#' output <- moead(problem = problem,
#'                 preset = configuration,
#'                 showpars = showpars, seed = seed)
#'
#' @export
preset_moead <- function(name = NULL) {

  presets <- list(
    original = list(
      decomp     = list(name       = "SLD", H = 99),
      neighbors  = list(name       = "lambda",
                        T          = 20,
                        delta.p    = 1),
      aggfun     = list(name       = "wt"),
      variation  = list(list(name  = "sbx",
                             etax  = 20, pc = 1),
                        list(name  = "polymut",
                             etam  = 20, pm = 0.1),
                        list(name  = "truncate")),
      update     = list(name       = "standard", UseArchive = FALSE),
      scaling    = list(name       = "none"),
      constraint = list(name       = "none"),
      stopcrit   = list(list(name  = "maxiter",
                             maxiter  = 200)))
  )

  if (is.null(name)) {
    output <- data.frame(name = c("\"original\"","\"dummy\""),
                         description = c("Original MOEA/D by Zhang and Li (2007)",
                                         "A non-existing MOEA/D configuration"))
    cat("Use `preset_moead([\"name\"])` to generate a standard MOEAD composition\n\n")

    format(output, justify = "left")
  }
  else {
    return(presets[name])
  }
}
