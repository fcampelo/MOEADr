rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
library(stringr)
library(ecr)
library(mco)
library(feather)
library(withr)
# lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)
# source("load.DTLZ.function.R")
# source("resource.allocation.R")
source("resource.allocation.R")
source("utils.R")
source("moead.R")

repetitions <- 1

algorithms <- c("moead.de")

#uniform weight
resource.allocation.DRA <- list(name = "DRA", dt = 20, selection = "tour")
resource.allocation.GRA <- list(name = "GRA", dt = 20, selection = "random")
resource.allocation.RAD <- list(name = "RAD", dt = 20, selection = "random")
resource.allocation.NORM <- list(name = "norm", dt = 20, selection = "random")
resource.allocation.RANDOM <- list(name = "random", dt = 20, selection = "random")


decomp <- list(name = "SLD", H = 149)
decomp2 <- list(name = "uniform", N = 150)
update2 <- list(name  = "onra")

scaling <- list()
scaling$name <- "simple"

n.objs <- c(2)

stopcrit  <- list(list(name    = "maxeval",
                       maxeval = 30000))

# update_GRA <- preset_moead(algo)$update
# update_GRA$name <- "onra"

for (n.obj in n.objs) {
  print(n.obj)
  fun.names1 <- list()
  for (i in 1:1) {#44 and 29
    fun.names1[[length(fun.names1) + 1]] = paste0("BiObjBBOB", i)
  }
  
  my.data <- data.frame()
  for (algo in algorithms) {
    print(algo)
    id = 1
    for (fun in fun.names1) {
      print(fun)
      problem <-
        makeBiObjBBOBFunction(dimension = 30,
                              fid = id,
                              iid = 1)
      id = id + 1
      problem.BiBBOB <- function(X) {
        t(apply(X, MARGIN = 1,
                FUN = problem))
      }
      
      par.set = ParamHelpers::getParamSet(problem)
      problem.zdt1 <- list(
        name       = "problem.BiBBOB",
        xmin       = as.numeric(getLower(par.set)),
        xmax       = as.numeric(getUpper(par.set)),
        m          = n.obj
      )
      # ref.points <- rep(1, problem.zdt1$m)
      
      for (j in 1:repetitions) {
        moead.de.data <- list()
        moead.dra.data <- list()
        moead.rad.data <- list()
        
        
        cat("rep:", j)
        moead.de <- moead(
          problem  = problem.zdt1,
          preset   = preset_moead("moead.de"),
          decomp = decomp,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 100),
          seed = j
        )
        
        moead.dra <- moead(
          problem  = problem.zdt1,
          preset   = preset_moead(algo),
          decomp = decomp,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 100),
          seed = j,
          resource.allocation = resource.allocation.DRA
        )
        
        
        # ondb
        moead.rad <- moead(
          problem  = problem.zdt1,
          preset   = preset_moead(algo),
          decomp = decomp,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j,
          resource.allocation = resource.allocation.RAD
        )
        
        moead.norm <- moead(
          problem  = problem.zdt1,
          preset   = preset_moead(algo),
          decomp = decomp,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j,
          resource.allocation = resource.allocation.NORM
        )

        moead.random <- moead(
          problem  = problem.zdt1,
          preset   = preset_moead(algo),
          decomp = decomp,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j,
          resource.allocation = resource.allocation.RANDOM
        )
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

