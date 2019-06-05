rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
library(stringr)
library(ecr)
library(mco)
library(feather)
library(pracma)
library(withr)
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)

repetitions <-  1
algorithms <- c("moead.de")

#uniform weight
resource.allocation.GRA <- list(name = "GRA", dt = 2)
resource.allocation.NORM <- list(name = "norm", dt = 1, selection = "random", type = "NULL")
resource.allocation.NORM.tour <- list(name = "norm", dt = 1, selection = "tour", type = "inverse", size = 0.2, k = 0.02)
resource.allocation.NORM.inverse <- list(name = "norm", dt = 1, selection = "random", type = "inverse")
resource.allocation.RANDOM <- list(name = "random", dt = 1)

update <- preset_moead("moead.de")$update
update$UseArchive <- TRUE


constraint.best.de = list(name = "penalty", beta=0.95)
variation <- preset_moead("moead.de")$variation

n.obj <- 3


stopcrit  <- list(list(name    = "maxeval",
                       maxeval = 30000))

scaling <- list()
scaling$name <- "simple"

d <- 2
fun <- "moon"
setwd("~/MOEADr/R/")
for (algo in algorithms) {
  print(algo)
  problem.zdt1 <- list(
    name       = "problem.moon",
    xmin       = rep(0, d),
    xmax       = rep(1, d),
    m          = n.obj,
    constraints = list(
      name      = "unitary_constraints",# constraint function routine
      epsilon   = 0.95) # tolerance for equality constraints
  )
  ref.points <- matrix(c(1.0, 0.0, 1.0), nrow = 1, ncol = 3)
  for (j in 1:repetitions) {
    decomp <- list(name = "SLD", H = 30)
    cat("rep:", j)
    
    moead.de <- moead(
      problem  = problem.zdt1,
      preset   = preset_moead(algo),
      decomp = decomp,
      variation = variation,
      stopcrit = stopcrit,
      constraint = constraint.best.de,
      scaling = scaling,
      showpars = list(show.iters = "none", showevery = 100),
      seed = j,
      update = update
    )
    
    moead.norm <- moead(
      problem  = problem.zdt1,
      preset   = preset_moead(algo),
      decomp = decomp,
      stopcrit = stopcrit,
      variation = variation,
      constraint = constraint.best.de,
      scaling = scaling,
      showpars = list(show.iters = "none", showevery = 10),
      seed = j,
      update = update,
      resource.allocation = resource.allocation.NORM
    )

    moead.norm.inverse <- moead(
      problem  = problem.zdt1,
      preset   = preset_moead(algo),
      decomp = decomp,
      stopcrit = stopcrit,
      variation = variation,
      constraint= constraint.best.de,
      scaling = scaling,
      showpars = list(show.iters = "none", showevery = 10),
      seed = j,
      update = update,
      resource.allocation = resource.allocation.NORM.inverse
    )
    # decomp <- list(name = "SLD", H = 30)
    moead.norm.tour <- moead(
      problem  = problem.zdt1,
      preset   = preset_moead(algo),
      decomp = decomp,
      stopcrit = stopcrit,
      variation = variation,
      constraint = constraint.best.de,
      scaling = scaling,
      showpars = list(show.iters = "none", showevery = 10),
      seed = j,
      update = update,
      resource.allocation = resource.allocation.NORM.tour
    )
  }
}
