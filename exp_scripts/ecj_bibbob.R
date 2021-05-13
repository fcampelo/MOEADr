rm(list = ls(all = TRUE))

library(smoof)
library(emoa)
library(mco)
library(feather)
library(eaf)
library(MOEADps)
library(parallel)
cores <- 1
cl <- makeCluster(cores)



source("~/MOEADr/R/moead_500.R")

# source("~/MOEADr/R/variation_diffmut_500.R")
# source("~/MOEADr/R/perform_variation_500.R")


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
    n = 1
  )

resource.allocation.50 <-
  list(
    name = "random",
    dt = 0,
    selection = "n",
    n = 48
  )



print("2 OBJECTIVES")
problem.to.solve <- (55:55)
for (fun in problem.to.solve) {
  problem <-
    smoof::makeBiObjBBOBFunction(dimension = dimensions,
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
    
  
    
    moead50 <- moeadps(
      problem  = problem.bibbob,
      preset   = preset_moead("moead.de"),
      decomp = decomp50,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors.50,
      showpars = list(show.iters = "none", showevery = 1000),
      seed = j + 422,
      update = update
    )
    
    savePlotData(
      moea = moead50,
      name = paste0(paste0(fun), "_moead50_", lambda, "_"),
      j = j,
      wd = "~/tec/"
    )
    rm(moead50)
    
    
    moead500 <- moeadps(
      problem  = problem.bibbob,
      preset   = preset_moead("moead.de"),
      decomp = decomp500,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors.500,
      showpars = list(show.iters = "none", showevery = 1000),
      seed = j + 422,
      update = update
    )
    
    savePlotData(
      moea = moead500,
      name = paste0(paste0(fun), "_moead500_", lambda, "_"),
      j = j,
      wd = "~/tec/"
    )
    rm(moead500)
    
    
    
    # moead.ps.1 <- moead(
    #   problem  = problem.bibbob,
    #   preset   = preset_moead("moead.de"),
    #   decomp = decomp500,
    #   variation = variation,
    #   stopcrit = stopcrit,
    #   scaling = scaling,
    #   neighbors = neighbors.500,
    #   showpars = list(show.iters = "none", showevery = 1000),
    #   update = update,
    #   resource.allocation = resource.allocation.1
    # )
    # savePlotData(
    #   moea = moead.1,
    #   name = paste0(paste0("bibbob_500_", fun), "_moead.ps.1_", lambda, "_"),
    #   j = j,
    #   wd = "~/tec/"
    # )
    # rm(moead.1)
    
    moead.ps.50 <- moeadps_500(
      problem  = problem.bibbob,
      preset   = preset_moead("moead.de"),
      decomp = decomp500,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors.500,
      showpars = list(show.iters = "none", showevery = 1000),
      update = update,
      resource.allocation = resource.allocation.50
    )
    savePlotData(
      moea = moead.ps.50,
      name = paste0(paste0(fun), "_moead.ps.50_", lambda, "_"),
      j = j,
      wd = "~/tec/"
    )
    rm(moead.random)
    
    
    
  }
  
}


