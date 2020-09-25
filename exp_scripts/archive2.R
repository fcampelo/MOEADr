# rm(list = ls(all = TRUE))
# setwd("~/MOEADr/R/")
library(smoof)
# library(MOEADr)
library(emoa)
library(feather)
# library(withr)
library(compiler)
library(ggplot2)
# source("~/MOEADr/R/utils.R")
# lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)
enableJIT(1)

library(MOEADps)


source("~/MOEADr/R/savePlotData.R")

repetitions <-  10
dimension <- 100
lambda <- 50

decomp <- list(name = "sld", H = 49, .nobj = 2)
loaded.weights.2objs <-
  data.matrix(
    read.csv(
      "~/MOEADr/SOBOL-2objs-500wei.ws",
      header = F,
      stringsAsFactors = FALSE,
      sep = " "
    )
  )
decomp.loaded.2 <- list(name = "loaded", W = loaded.weights.2objs)
variation = preset_moead("moead.de")$variation
variation[[2]]$pm = 1 / dimension
scaling <- list()
scaling$name <- "simple"
neighbors <- preset_moead("moead.de")$neighbors
neighbors$T <- 100
neighbors3 <- preset_moead("moead.de")$neighbors
neighbors3$T <- 10
update <- preset_moead("moead.de")$update
update$UseArchive = TRUE

stopcrit  <- list(list(name    = "maxeval",
                       maxeval = 30000))



n.obj <- 2




print("2 OBJECTIVES")
problem.to.solve <- c("UF1", "UF2","UF3", "UF4", "UF5", "UF6", "UF7")
for (fun in problem.to.solve) {
  
  benchmark <- strsplit(fun, "[0-9]")[[1]][1]
  number <- as.integer(strsplit(fun, "[A-Z]")[[1]][3])
  problem.smoof.UF <-
    makeUFFunction(dimension = dimension,
                   id = number)
  problem.UF <- function(X) {
    t(apply(X, MARGIN = 1,
            FUN = problem.smoof.UF))
  }
  par.set = ParamHelpers::getParamSet(problem.smoof.UF)
  problem.uf <- list(
    name       = "problem.UF",
    xmin       = as.numeric(getLower(par.set)),
    xmax       = as.numeric(getUpper(par.set)),
    m          = n.obj
  )
  for (j in 1:repetitions) {
    cat("rep", j, "\n")
    
    resource.allocation.RANDOM <-
      list(
        name = "random",
        dt = 0,
        selection = "n",
        n = lambda
      )
    
    # update$nr <- 1
    # moead.smallT <- moead(
    #   problem  = problem.uf,
    #   preset   = preset_moead("moead.de"),
    #   decomp = decomp.loaded.2,
    #   variation = variation,
    #   stopcrit = stopcrit,
    #   scaling = scaling,
    #   neighbors = neighbors,
    #   showpars = list(show.iters = "none", showevery = 1000),
    #   seed = j + 422,
    #   update = update,
    #   resource.allocation = resource.allocation.RANDOM,
    #   small.update = TRUE
    # )
    # 
    # savePlotData(
    #   moea = moead.smallT,
    #   name = paste0(fun, "_moead.smallT_", lambda, "_"),
    #   j = j,
    #   wd = "~/jsec_2020_50/"
    # )
    # rm(moead.smallT)
    exit()
    update$nr <- 2
    moead.random <- moead(
      problem  = problem.uf,
      preset   = preset_moead("moead.de"),
      decomp = decomp.loaded.2,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors,
      showpars = list(show.iters = "none", showevery = 1000),
      seed = j + 422,
      update = update,
      resource.allocation = resource.allocation.RANDOM
    )
    
    savePlotData(
      moea = moead.random,
      name = paste0(fun, "_moead.random_", lambda, "_"),
      j = j,
      wd = "~/jsec_2020_50/"
    )
    rm(moead.random)
    
    moead.3 <- moead(
      problem  = problem.uf,
      preset   = preset_moead("moead.de"),
      decomp = decomp,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors3,
      showpars = list(show.iters = "none", showevery = 1000),
      seed = j + 422,
      update = update
    )
    
    savePlotData(
      moea = moead.3,
      name = paste0(fun, "_moead.3_", lambda, "_"),
      j = j,
      wd = "~/jsec_2020_50/"
    )
    rm(moead.3)
    
    
    moead500 <- moead(
      problem  = problem.uf,
      preset   = preset_moead("moead.de"),
      decomp = decomp.loaded.2,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors,
      showpars = list(show.iters = "none", showevery = 1000),
      seed = j + 422,
      update = update
    )
    
    savePlotData(
      moea = moead500,
      name = paste0(fun, "_moead500_", lambda, "_"),
      j = j,
      wd = "~/jsec_2020_50/"
    )
    rm(moead500)
  }
}


