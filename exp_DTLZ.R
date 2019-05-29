rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
# library(stringr)
# library(ecr)
# library(mco)
library(feather)
library(withr)
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)
# source("load.DTLZ.function.R")
# source("resource.allocation.R")
# source("utils.R")
# source("moead.R")

repetitions <-  7
dimension <- 100
algorithms <- c("moead.de")

#uniform weightNUL
resource.allocation.DRA <- list(name = "DRA", dt = 20, selection = "dra", type = "NULL")
resource.allocation.GRA <- list(name = "GRA", dt = 20, selection = "random", type = "NULL")
resource.allocation.RAD <- list(name = "RAD", dt = 20, selection = "random", type = "NULL")
resource.allocation.NORM <- list(name = "norm", dt = 20, selection = "random", type = "NULL")
resource.allocation.NORM.tour <- list(name = "norm", dt = 20, selection = "tour", type = "NULL")
resource.allocation.NORM.inverse <- list(name = "norm", dt = 20, selection = "random", type = "inverse")
resource.allocation.RANDOM <- list(name = "random", dt = 20, selection = "random", type = "NULL")


decomp <- list(name = "SLD", H = 349)

scaling <- list()
scaling$name <- "simple"

n.objs <- c(2)


stopcrit  <- list(list(name    = "maxeval",
                       maxeval = 30000))

for (n.obj in n.objs) {
  id <- 1
  print(n.obj)
  if (n.obj==3) decomp <- list(name = "SLD", H = 25)
  fun.names1 <- list()
  for (i in 4:4) {
    fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
  }
  
  my.data <- data.frame()
  for (algo in algorithms) {
    print(algo)
    for (fun in fun.names1) {
      print(fun)
      problem <-
        load.DTLZ.function(fun, dimension = dimension, n.obj = n.obj
        )
      problem <- problem$fn
      problem.DTLZ <- function(X) {
        t(apply(X, MARGIN = 1,
                FUN = problem))
      }
      
      par.set = ParamHelpers::getParamSet(problem)
      problem.zdt1 <- list(
        name       = "problem.DTLZ",
        xmin       = as.numeric(getLower(par.set)),
        xmax       = as.numeric(getUpper(par.set)),
        m          = n.obj
      )
      # ref.points <- rep(1, problem.zdt1$m)
      
      for (j in 1:repetitions) {
        moead.de.data <- list()
        moead.dra.data <- list()
        moead.gra.data <- list()
        moead.rad.data <- list()
        nsga.2.data <- list()
        
        
        cat("rep:", j)
        
  
        # moead.de <- moead(
        #   problem  = problem.zdt1,
        #   preset   = preset_moead(algo),
        #   decomp = decomp,
        #   stopcrit = stopcrit,
        #   scaling = scaling,
        #   showpars = list(show.iters = "none", showevery = 100),
        #   seed = j
        # )
        # 
        # moead.dra <- moead(
        #   problem  = problem.zdt1,
        #   preset   = preset_moead(algo),
        #   decomp = decomp,
        #   stopcrit = stopcrit,
        #   scaling = scaling,
        #   showpars = list(show.iters = "none", showevery = 100),
        #   seed = j,
        #   resource.allocation = resource.allocation.DRA
        # )
        # 
        # 
        # # gra.awt
        # moead.gra <- moead(
        #   problem  = problem.zdt1,
        #   preset   = preset_moead(algo),
        #   decomp = decomp,
        #   stopcrit = stopcrit,
        #   scaling = scaling,
        #   showpars = list(show.iters = "none", showevery = 100),
        #   seed = j,
        #   resource.allocation = resource.allocation.GRA,
        #   my.file.n = my.file.n
        # )
        # 
        # moead.rad <- moead(
        #   problem  = problem.zdt1,
        #   preset   = preset_moead(algo),
        #   decomp = decomp,
        #   stopcrit = stopcrit,
        #   scaling = scaling,
        #   showpars = list(show.iters = "none", showevery = 10),
        #   seed = j,
        #   resource.allocation = resource.allocation.RAD
        # )
        
        # moead.norm <- moead(
        #   problem  = problem.zdt1,
        #   preset   = preset_moead(algo),
        #   decomp = decomp,
        #   stopcrit = stopcrit,
        #   scaling = scaling,
        #   showpars = list(show.iters = "none", showevery = 10),
        #   seed = j,
        #   resource.allocation = resource.allocation.NORM
        # )
        # 
        # savePlotData(moea = moead.norm, name = "moead.norm", j = j)
        # 
        # moead.norm.inverse <- moead(
        #   problem  = problem.zdt1,
        #   preset   = preset_moead(algo),
        #   decomp = decomp,
        #   stopcrit = stopcrit,
        #   scaling = scaling,
        #   showpars = list(show.iters = "none", showevery = 10),
        #   seed = j,
        #   resource.allocation = resource.allocation.NORM.inverse
        # )
        # 
        # savePlotData(moea = moead.norm.inverse, name = "moead.norm.inverse", j = j)
        
        moead.norm.tournament <- moead(
          problem  = problem.zdt1,
          preset   = preset_moead(algo),
          decomp = decomp,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j,
          resource.allocation = resource.allocation.NORM.tour
        )
        
        savePlotData(moea = moead.norm.tournament, name = "moead.norm.tournament", j = j)
        
        # moead.random <- moead(
        #   problem  = problem.zdt1,
        #   preset   = preset_moead(algo),
        #   decomp = decomp,
        #   stopcrit = stopcrit,
        #   scaling = scaling,
        #   showpars = list(show.iters = "none", showevery = 10),
        #   seed = j,
        #   resource.allocation = resource.allocation.RANDOM
        # )
        # exit()
        # 
        # 
        # # par.set = ParamHelpers::getParamSet(problem)
        # nsga.2 = nsga2(
        #   problem,
        #   idim = getNumberOfParameters(problem),
        #   generations = moead.de$n.iter,
        #   odim = n.obj,
        #   lower.bounds = as.numeric(getLower(par.set)),
        #   upper.bounds = as.numeric(getUpper(par.set)),
        #   popsize = (floor(dim(moead.de$W)[1]/4))*4
        # ) 
      }
    }
  }
}
