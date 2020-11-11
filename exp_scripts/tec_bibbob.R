rm(list = ls(all = TRUE))

library(smoof)
library(emoa)
library(mco)
library(feather)
# library(eaf)
library(MOEADps)

source("~/MOEADr/R/utils.R")
source("~/MOEADr/R/load.DTLZ.function.R")

repetitions <-  10
dimensions <- 40
lambda <- 50

loaded.weights.50 <-
  data.matrix(
    read.csv(
      "~/MOEADr/weights-sobol/SOBOL-2objs-50wei.ws",
      header = F,
      stringsAsFactors = FALSE,
      sep = " "
    )
  )
decomp50 <- list(name = "loaded", W = loaded.weights.50)

loaded.weights.500 <-
  data.matrix(
    read.csv(
      "~/MOEADr/weights-sobol/SOBOL-2objs-500wei.ws",
      header = F,
      stringsAsFactors = FALSE,
      sep = " "
    )
  )
decomp500 <- list(name = "loaded", W = loaded.weights.500)

variation = preset_moead("moead.de")$variation
variation[[2]]$pm = 1 / dimensions
scaling <- list()
scaling$name <- "simple"
neighbors.500 <- preset_moead("moead.de")$neighbors
neighbors.500$T <- 100
neighbors.50 <- preset_moead("moead.de")$neighbors
neighbors.50$T <- 10
update <- preset_moead("moead.de")$update
update$UseArchive = TRUE

stopcrit  <- list(list(name    = "maxeval",
                       maxeval = 100000))



n.obj <- 2

resource.allocation.1 <-
  list(
    name = "random",
    dt = 0,
    selection = "n",
    n = 1 + n.obj
  )

resource.allocation.50 <-
  list(
    name = "random",
    dt = 0,
    selection = "n",
    n = 50 + n.obj
  )



print("2 OBJECTIVES")
problem.to.solve <- (1:55)
for (fun in problem.to.solve) {
  problem <-
    smoof::makeBiObjBBOBFunction(dimensions = dimensions,
                                 fid = fun,
                                 iid = 1)
  problem.BiBBOB <- function(X) {
    t(apply(X, MARGIN = 1,
            FUN = problem))
  }
  
  par.set = ParamHelpers::getParamSet(problem)
  problem.bibbob <- list(
    name       = "problem.BiBBOB",
    xmin       = as.numeric(getLower(par.set)),
    xmax       = as.numeric(getUpper(par.set)),
    m          = n.obj
  )
  
  
  
  
  for (j in 1:repetitions) {
    cat("rep", j, "\n")
    
    seed <- sample(1:1000)[1]
  
    
    dir.name <- paste0("~/tec/moead50_", j, "/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)
    
    moead50 <- moeadps(
      problem  = problem.bibbob,
      preset   = preset_moead("moead.de"),
      decomp = decomp50,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors.50,
      showpars = list(show.iters = "none", showevery = 1000),
      seed = seed,
      update = update,
      saving.dir = dir.name
    )
    
    rm(moead50)
    
    dir.name <- paste0("~/tec/moead500_", j, "/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)
    
    moead500 <- moeadps(
      problem  = problem.bibbob,
      preset   = preset_moead("moead.de"),
      decomp = decomp500,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors.500,
      showpars = list(show.iters = "none", showevery = 1000),
      seed = seed,
      update = update,
      saving.dir = dir.name
    )
    
    rm(moead500)
    
    dir.name <- paste0("~/tec/moead.ps.1_", j, "/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)
    
    moead.ps.1 <- moeadps(
      problem  = problem.bibbob,
      preset   = preset_moead("moead.de"),
      decomp = decomp500,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors.500,
      showpars = list(show.iters = "none", showevery = 1000),
      update = update,
      resource.allocation = resource.allocation.1,
      seed = seed,
      saving.dir = dir.name
    )
    rm(moead.1)
    
    dir.name <- paste0("~/tec/moead.ps.50_", j, "/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)
    
    moead.ps.50 <- moeadps(
      problem  = problem.bibbob,
      preset   = preset_moead("moead.de"),
      decomp = decomp500,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors.500,
      showpars = list(show.iters = "none", showevery = 1000),
      update = update,
      resource.allocation = resource.allocation.50,
      seed = seed,
      saving.dir = dir.name
    )
    
    rm(moead.ps.50)
    
    
    
  }
  
}
