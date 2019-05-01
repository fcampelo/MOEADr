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
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)
# source("load.DTLZ.function.R")
# source("resource.allocation.R")
# source("utils.R")
# source("moead.R")

repetitions <- 21

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
  for (i in 44:44) {#44 and 29
    fun.names1[[length(fun.names1) + 1]] = paste0("BiObjBBOB", i)
  }
  
  my.data <- data.frame()
  for (algo in algorithms) {
    print(algo)
    id = 44
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
        
        my.file.n <- paste0("../../de/",fun,"_")
        moead.de <- moead(
          problem  = problem.zdt1,
          preset   = preset_moead(algo),
          decomp = decomp,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 100),
          seed = j,
          my.file.n = my.file.n
        )
        
        my.file.n <- paste0("../../dra/",fun,"_")
        moead.dra <- moead(
          problem  = problem.zdt1,
          preset   = preset_moead(algo),
          decomp = decomp,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 100),
          seed = j,
          resource.allocation = resource.allocation.DRA,
          my.file.n = my.file.n
        )
        
        
        # gra.awt
        # my.file.n <- paste0("../../gra/",fun,"_")
        # moead.gra <- moead(
        #   problem  = problem.zdt1,
        #   preset   = preset_moead(algo),
        #   decomp = decomp2,
        #   stopcrit = stopcrit,
        #   scaling = scaling,
        #   showpars = list(show.iters = "none", showevery = 100),
        #   seed = j,
        #   update = update2,
        #   resource.allocation = resource.allocation.GRA,
        #   my.file.n = my.file.n
        # )
        
        # write_feather(as.data.frame(moead.gra$Y), my.file.n)
        
        # ondb
        my.file.n <- paste0("../../rad/",fun,"_")
        moead.rad <- moead(
          problem  = problem.zdt1,
          preset   = preset_moead(algo),
          decomp = decomp,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j,
          resource.allocation = resource.allocation.RAD,
          my.file.n = my.file.n
        )
        
        # my.file.n <- paste0("../../norm_",fun,"/")
        # moead.norm <- moead(
        #   problem  = problem.zdt1,
        #   preset   = preset_moead(algo),
        #   decomp = decomp,
        #   stopcrit = stopcrit,
        #   scaling = scaling,
        #   showpars = list(show.iters = "none", showevery = 10),
        #   seed = j,
        #   resource.allocation = resource.allocation.NORM,
        #   my.file.n = my.file.n
        # )
        
        # my.file.n <- paste0("../../random_",fun,"/")
        # moead.random <- moead(
        #   problem  = problem.zdt1,
        #   preset   = preset_moead(algo),
        #   decomp = decomp,
        #   update = update,
        #   stopcrit = stopcrit,
        #   scaling = scaling,
        #   variation = variation,
        #   showpars = list(show.iters = "none", showevery = 10),
        #   seed = j,
        #   resource.allocation = resource.allocation.RANDOM,
        #   my.file.n = my.file.n
        # )
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

