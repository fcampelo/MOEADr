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

repetitions <-  21
dimension <- 30
algorithms <- c("moead.de")

#uniform weight
resource.allocation.DRA <- list(name = "DRA", dt = 20)
resource.allocation.GRA <- list(name = "GRA", dt = 20)
resource.allocation.RAD <- list(name = "RAD", dt = 20)
resource.allocation.NORM <- list(name = "norm", dt = 20)
resource.allocation.RANDOM <- list(name = "random", dt = 20)



decomp <- list(name = "SLD", H = 25)

n.obj <- 3


stopcrit  <- list(list(name    = "maxeval",
                       maxeval = 140000))

scaling <- list()
scaling$name <- "simple"

d <- 2
fun <- "moon"
my.data <- data.frame()
for (algo in algorithms) {
  # print(algo)
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
    
    cat("rep:", j)
    
    
    my.file.n <- paste0("../../de/",fun,"_")
    moead.de <- moead(
      problem  = problem.zdt1,
      preset   = preset_moead(algo),
      decomp = decomp,
      stopcrit = stopcrit,
      constraint = list(name = "penalty", beta=0.95),
      scaling = scaling,
      showpars = list(show.iters = "none", showevery = 100),
      seed = j,
      my.file.n = my.file.n
    )
    
    
    # gra.awt
    my.file.n <- paste0("../../gra/",fun,"_")
    moead.gra <- moead(
      problem  = problem.zdt1,
      preset   = preset_moead(algo),
      decomp = decomp,
      stopcrit = stopcrit,
      constraint = list(name = "penalty", beta=0.95),
      scaling = scaling,
      showpars = list(show.iters = "none", showevery = 100),
      seed = j,
      resource.allocation = resource.allocation.GRA,
      my.file.n = my.file.n
    )
    
    # write_feather(as.data.frame(moead.gra$Y), my.file.n)
    
    # ondb
    # my.file.n <- paste0("../../rad/",fun,"_")
    # moead.rad <- moead(
    #   problem  = problem.zdt1,
    #   preset   = preset_moead(algo),
    #   decomp = decomp,
    #   stopcrit = stopcrit,
    #   scaling = scaling,
    #   showpars = list(show.iters = "none", showevery = 10),
    #   seed = j,
    #   resource.allocation = resource.allocation.RAD,
    #   my.file.n = my.file.n
    # )
    #
    my.file.n <- paste0("../../norm/",fun,"_")
    moead.norm <- moead(
      problem  = problem.zdt1,
      preset   = preset_moead(algo),
      decomp = decomp,
      stopcrit = stopcrit,
      constraint = list(name = "penalty", beta=0.95),
      scaling = scaling,
      showpars = list(show.iters = "none", showevery = 10),
      seed = j,
      resource.allocation = resource.allocation.NORM,
      my.file.n = my.file.n
    )
    
    my.file.n <- paste0("../../random/",fun,"_")
    moead.random <- moead(
      problem  = problem.zdt1,
      preset   = preset_moead(algo),
      decomp = decomp,
      stopcrit = stopcrit,
      constraint = list(name = "penalty", beta=0.95),
      scaling = scaling,
      showpars = list(show.iters = "none", showevery = 10),
      seed = j,
      resource.allocation = resource.allocation.RANDOM,
      my.file.n = my.file.n
    )
  
  }
}
