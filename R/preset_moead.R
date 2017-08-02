#' preset_moead
#'
#' Standard MOEA/D Preset (Zhang and Li 2007)
#'
#' Returns a configuration object that replicates the
#' MOEA/D as in Zhang and Li (2007) (sec. V-E, p.721-722)
#'
#' @export
preset_moead <- function() {
  moead.preset <- list(
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
}
