#' preset_moead
#'
#' Generate a preset configuration for moead()].
#'
#' This function returns a list of configuration presets taken from
#' the literature to be used with the [moead()] function in package `MOEADr`.
#'
#' Use these configurations as a starting point. We strongly
#' recommend that you play around with the particular configurations
#' (see example).
#'
#' @param name name of the preset to be generated. Use `preset_moead()` to obtain
#'             the list of available options.
#'
#' @return List object containing the preset, to be used as an input to [moead()];
#'         or, if `name == NULL` (the default), returns a logical flag invisibly.
#'
#' @examples
#'
#' # Generate list of available presets
#' preset_moead(name = NULL)
#'
#' \dontrun{
#'   library(smoof) # < Install package smoof if needed
#'   ZDT1 <- make_vectorized_smoof(prob.name  = "ZDT1",
#'                                 dimensions = 30)
#'                                 problem   <- list(name       = "ZDT1",
#'                                                   xmin       = rep(0, 30),
#'                                                   xmax       = rep(1, 30),
#'                                                   m          = 2)
#'
#'   # Get preset configuration for original MOEA/D
#'   configuration <- preset_moead("original")
#'
#'   # Modify whatever you fancy:
#'   stopcrit <- list(list(name = "maxiter", maxiter = 50))
#'   showpars <- list(show.iters = "dots", showevery  = 10)
#'   seed     <- 42
#'
#'   output <- moead(problem  = problem,
#'                   preset   = configuration,
#'                   showpars = showpars,
#'                   stopcrit = stopcrit,
#'                   seed     = seed)
#' }
#'
#' @export

preset_moead <- function(name = NULL) {

  # ===========================================================================
  # 1. Enter preset definitions below.

  # Original MOEA/D: Zhang and Li (2007), Sec. V-E, p.721-722
  original <- list(
    description = "Original MOEA/D: Zhang and Li (2007) (sec. V-E, p.721-722)",
    decomp      = list(name          = "SLD", H = 99),
    neighbors   = list(name          = "lambda",
                       T             = 20,
                       delta.p       = 1),
    aggfun      = list(name          = "wt"),
    variation   = list(list(name     = "sbx",
                            etax     = 20, pc = 1),
                       list(name     = "polymut",
                            etam     = 20, pm = 0.1),
                       list(name     = "truncate")),
    update      = list(name          = "standard", UseArchive = FALSE),
    scaling     = list(name          = "none"),
    constraint  = list(name          = "none"),
    stopcrit    = list(list(name     = "maxiter",
                            maxiter  = 200)))

  # ========================================
  # Original MOEA/D-DE: Zhang and Li (2007), Sec. V-E, p.721-722
  original <- list(
    description = "Original MOEA/D: Zhang and Li (2007), Sec. V-E, p.721-722",
    decomp      = list(name          = "SLD",
                       H             = 99),
    neighbors   = list(name          = "lambda",
                       T             = 20,
                       delta.p       = 1),
    aggfun      = list(name          = "wt"),
    variation   = list(list(name     = "sbx",
                            etax     = 20,
                            pc       = 1),
                       list(name     = "polymut",
                            etam     = 20,
                            pm       = 0.1),
                       list(name     = "truncate")),
    update      = list(name          = "standard",
                       UseArchive    = FALSE),
    scaling     = list(name          = "none"),
    constraint  = list(name          = "none"),
    stopcrit    = list(list(name     = "maxiter",
                            maxiter  = 200)))


  # ========================================
  original2 <- list(
    description = "Original MOEA/D, v2: Zhang and Li (2007), Sec. V-F, p.724",
    decomp      = list(name          = "SLD",
                       H             = 99),
    neighbors   = list(name          = "lambda",
                       T             = 20,
                       delta.p       = 1),
    aggfun      = list(name          = "PBI",
                       theta         = 5),
    variation   = list(list(name     = "sbx",
                            etax     = 20,
                            pc       = 1),
                       list(name     = "polymut",
                            etam     = 20,
                            pm       = 0.1),
                       list(name     = "truncate")),
    update      = list(name          = "standard",
                       UseArchive    = FALSE),
    scaling     = list(name          = "simple"),
    constraint  = list(name          = "none"),
    stopcrit    = list(list(name     = "maxiter",
                            maxiter  = 200)))


  # ========================================
  moead.de <- list(
    description = "MOEA/D-DE: Li and Zhang (2009)",
    decomp      = list(name          = "SLD",
                       H             = 299),
    neighbors   = list(name          = "lambda",
                       T             = 20,
                       delta.p       = 0.9),
    aggfun      = list(name          = "wt"),
    variation   = list(list(name     = "diffmut",
                            basis    = "rand",
                            phi      = 0.5),
                       list(name     = "polymut",
                            etam     = 20,
                            pm       = 1/30),
                       list(name     = "truncate")),
    update      = list(name          = "restricted",
                       nr            = 2,
                       UseArchive    = FALSE),
    scaling     = list(name          = "none"),
    constraint  = list(name          = "none"),
    stopcrit    = list(list(name     = "maxiter",
                            maxiter  = 300)))

  # ===========================================================================
  # 2. Get all preset names
  var.names <- names(as.list(environment()))
  var.names <- var.names[(length(var.names) - 1):1]

  if (is.null(name)) {
    descriptions <- unlist(lapply(X   = var.names,
                                  FUN = function(x){get(x)$description}))

    output <- data.frame(name        = var.names,
                         x           = rep("|", length(var.names)),
                         description = descriptions)
    print(format(output, justify = "left"))
    cat("\n\nUse preset_moead(\"name\") to generate a standard MOEAD composition\n\n")
    invisible(TRUE)
  }
  else {
    if(name %in% var.names){
      preset <- get(name)
      preset$description <- NULL
      return(preset)
    } else stop("Preset ", name, "not defined.")
  }
}
