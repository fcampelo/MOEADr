rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
library(feather)
library(withr)
# library(nsga2R)
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)


repetitions <-  9
dimension <- 100

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
update <- preset_moead("moead.de")$update
update$UseArchive = TRUE

stopcrit  <- list(list(name    = "maxeval",
                       maxeval = 1000))



n.obj <- 2
problem <-
  load.DTLZ.function("DTLZ7", dimension = dimension, n.obj = n.obj)
problem.smoof.DTLZ <- problem$fn
problem.DTLZ <- function(X) {
  t(apply(X, MARGIN = 1,
          FUN = problem.smoof.DTLZ))
}
par.set = ParamHelpers::getParamSet(problem.smoof.DTLZ)
problem.dtlz7 <- list(
  name       = "problem.DTLZ",
  xmin       = as.numeric(getLower(par.set)),
  xmax       = as.numeric(getUpper(par.set)),
  m          = n.obj
)



print("2 OBJECTIVES")
fun <- "DTLZ7"
saving_number <- 1
for (j in 83463:(83463 + repetitions)) {
  number_subproblems <-
    c(3, 4, 5, 6, 7, 8, 9, 10, 30, 50, 100, 150, 250)
  
  for (lambda in number_subproblems) {
    resource.allocation.RI <-
      list(
        name = "RI",
        dt = 1,
        selection = "n",
        n = lambda
      )
    
    resource.allocation.NORM <-
      list(
        name = "norm",
        dt = 1,
        selection = "n",
        n = lambda
      )
    
    resource.allocation.RANDOM <-
      list(
        name = "random",
        dt = 0,
        selection = "n",
        n = lambda
      )
    sampled <- sample(1:3)
    i <- 1
    while (i <= 3) {
      if (sampled[i] == 1) {
        moead.RI <- moead(
          problem  = problem.dtlz7,
          preset   = preset_moead("moead.de"),
          neighbors = neighbors,
          decomp = decomp.loaded.2,
          variation = variation,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 100),
          seed = j,
          update = update,
          resource.allocation = resource.allocation.RI,
          loaded.weights = loaded.weights.2objs
        )
        savePlotData(
          moea = moead.RI,
          name = paste0(fun, "_moead.RI_", lambda, "_"),
          j = saving_number,
          wd = "~/france_data/"
        )
      }
      else if (sampled[i] == 3) {
        moead.norm <- moead(
          problem  = problem.dtlz7,
          preset   = preset_moead("moead.de"),
          decomp = decomp.loaded.2,
          variation = variation,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j,
          update = update,
          resource.allocation = resource.allocation.NORM,
          loaded.weights = loaded.weights.2objs
        )
        
        savePlotData(
          moea = moead.norm,
          name = paste0(fun, "_moead.norm_", lambda, "_"),
          j = saving_number,
          wd = "~/france_data/"
        )
      }
      else if (sampled[i] == 2) {
        moead.random <- moead(
          problem  = problem.dtlz7,
          preset   = preset_moead("moead.de"),
          decomp = decomp.loaded.2,
          variation = variation,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j,
          update = update,
          resource.allocation = resource.allocation.RANDOM,
          loaded.weights = loaded.weights.2objs
        )
        
        savePlotData(
          moea = moead.random,
          name = paste0(fun, "_moead.random_", lambda, "_"),
          j = saving_number,
          wd = "~/france_data/"
        )
      }
      i <- i + 1
    }
  }
  saving_number <- saving_number + 1
}

print("3 OBJECTIVES")

fun <- "DTLZ7"
problem.smoof.UF <-
  makeUFFunction(dimension = dimension,
                 id = 9)
problem.UF <- function(X) {
  t(apply(X, MARGIN = 1,
          FUN = problem.smoof.UF))
}
par.set = ParamHelpers::getParamSet(problem.smoof.UF)
problem.uf9 <- list(
  name       = "problem.UF",
  xmin       = as.numeric(getLower(par.set)),
  xmax       = as.numeric(getUpper(par.set)),
  m          = n.obj
)

loaded.weights.3objs <-
  data.matrix(
    read.csv(
      "~/MOEADr/SOBOL-3objs-500wei.ws",
      header = F,
      stringsAsFactors = FALSE,
      sep = " "
    )
  )
decomp.loaded.3 <- list(name = "loaded", W = loaded.weights.3objs)


saving_number <- 1
for (j in 83463:(83463 + repetitions)) {
  number_subproblems <-
    c(3, 4, 5, 6, 7, 8, 9, 10, 30, 50, 100, 150, 250)
  
  for (lambda in number_subproblems) {
    resource.allocation.RI <-
      list(
        name = "RI",
        dt = 1,
        selection = "n",
        n = lambda
      )
    
    resource.allocation.NORM <-
      list(
        name = "norm",
        dt = 1,
        selection = "n",
        n = lambda
      )
    
    resource.allocation.RANDOM <-
      list(
        name = "random",
        dt = 0,
        selection = "n",
        n = lambda
      )
    
    
    sampled <- sample(1:3)
    i <- 1
    while (i <= 3) {
      if (sampled[i] == 1) {
        moead.RI <- moead(
          problem  = problem.uf9,
          preset   = preset_moead("moead.de"),
          neighbors = neighbors,
          decomp = decomp.loaded.3,
          variation = variation,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 100),
          seed = j,
          update = update,
          resource.allocation = resource.allocation.RI,
          loaded.weights = loaded.weights.3objs
        )
        savePlotData(
          moea = moead.RI,
          name = paste0(fun, "_moead.RI_", lambda, "_"),
          j = saving_number,
          wd = "~/france_data/"
        )
      }
      else if (sampled[i] == 3) {
        moead.norm <- moead(
          problem  = problem.uf9,
          preset   = preset_moead("moead.de"),
          decomp = decomp.loaded.3,
          variation = variation,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j,
          update = update,
          resource.allocation = resource.allocation.NORM,
          loaded.weights = loaded.weights.3objs
        )
        
        savePlotData(
          moea = moead.norm,
          name = paste0(fun, "_moead.norm_", lambda, "_"),
          j = saving_number,
          wd = "~/france_data/"
        )
      }
      else if (sampled[i] == 2) {
        moead.random <- moead(
          problem  = problem.uf9,
          preset   = preset_moead("moead.de"),
          decomp = decomp.loaded.3,
          variation = variation,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j,
          update = update,
          resource.allocation = resource.allocation.RANDOM,
          loaded.weights = loaded.weights.3objs
        )
        
        savePlotData(
          moea = moead.norm,
          name = paste0(fun, "_moead.random_", lambda, "_"),
          j = saving_number,
          wd = "~/france_data/"
        )
      }
      i <- i + 1
    }
  }
  saving_number <- saving_number + 1
}