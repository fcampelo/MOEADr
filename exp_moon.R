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

repetitions <- 21
algorithms <- c("moead.de")

#uniform weight
resource.allocation.GRA <- list(name = "GRA", dt = 2)
resource.allocation.NORM <- list(name = "norm", dt = 1, selection = "random", type = "NULL")
resource.allocation.NORM.tour <- list(name = "norm", dt = 1, selection = "tour", type = "NULL", size = 0.2, k = 0.02)
resource.allocation.NORM.inverse <- list(name = "norm", dt = 1, selection = "random", type = "inverse")
resource.allocation.RANDOM <- list(name = "random", dt = 1)

constraint.best.de = list(name = "penalty", beta=0.95)

n.obj <- 3


stopcrit  <- list(list(name    = "maxeval",
                       maxeval = 60000))

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
  
    problem <- list(
    name       = "problem.moon",
    xmin       = rep(0, d),
    xmax       = rep(1, d),
    m          = n.obj# tolerance for equality constraints
  )
  for (j in (1+30):(repetitions+30)) {
decomp <- list(name = "SLD", H = 50)
    cat("rep:", j)
    
    # moead.de <- moead(
    #   problem  = problem,
    #   preset   = preset_moead(algo),
    #   decomp = decomp,
    #   stopcrit = stopcrit,
    #   scaling = scaling,
    #   showpars = list(show.iters = "none", showevery = 100),
    #   seed = j
    # )
    # savePlotData(moea = moead.de, name = paste0(fun,"moead.de"), j = j)
    
    moead.de.c <- moead(
      problem  = problem.zdt1,
      preset   = preset_moead(algo),
      decomp = decomp,
      stopcrit = stopcrit,
      constraint = constraint.best.de,
      scaling = scaling,
      showpars = list(show.iters = "none", showevery = 100),
      seed = j
    )
    moead.de.c$X <- moead.de.c$u.archive$X
    moead.de.c$Y <- moead.de.c$u.archive$Y
    moead.de.c$V$Vmatrix <- moead.de.c$u.archive$Vmatrix
    savePlotData(moea = moead.de.c, name = paste0(fun,"moead.de.c"), j = j)
    
    moead.norm <- moead(
      problem  = problem.zdt1,
      preset   = preset_moead(algo),
      decomp = decomp,
      stopcrit = stopcrit,
      constraint = constraint.best.de,
      scaling = scaling,
      showpars = list(show.iters = "none", showevery = 10),
      seed = j,
      resource.allocation = resource.allocation.NORM
    )
    moead.norm$X <- moead.norm$u.archive$X
    moead.norm$Y <- moead.norm$u.archive$Y
    moead.norm$V$Vmatrix <- moead.norm$u.archive$Vmatrix
    savePlotData(moea = moead.norm, name = paste0(fun,"moead.norm"), j = j)
    
    moead.norm.inverse <- moead(
      problem  = problem.zdt1,
      preset   = preset_moead(algo),
      decomp = decomp,
      stopcrit = stopcrit,
      constraint= constraint.best.de,
      scaling = scaling,
      showpars = list(show.iters = "none", showevery = 10),
      seed = j,
      resource.allocation = resource.allocation.NORM.inverse
    )
    moead.norm.inverse$X <- moead.norm.inverse$u.archive$X
    moead.norm.inverse$Y <- moead.norm.inverse$u.archive$Y
    moead.norm.inverse$V$Vmatrix <- moead.norm.inverse$u.archive$Vmatrix
    savePlotData(moea = moead.norm.inverse, name = paste0(fun,"moead.norm.inverse"), j = j)
    
    moead.norm.tour <- moead(
      problem  = problem.zdt1,
      preset   = preset_moead(algo),
      decomp = decomp,
      stopcrit = stopcrit,
      constraint = constraint.best.de,
      scaling = scaling,
      showpars = list(show.iters = "none", showevery = 10),
      seed = j,
      resource.allocation = resource.allocation.NORM.tour
    )
    moead.norm.tour$X <- moead.norm.tour$u.archive$X
    moead.norm.tour$Y <- moead.norm.tour$u.archive$Y
    moead.norm.tour$V$Vmatrix <- moead.norm.tour$u.archive$Vmatrix
    savePlotData(moea = moead.norm.tour, name = paste0(fun,"moead.norm.tour"), j = j)
  }
}
