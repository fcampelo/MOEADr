rm(list = ls(all = TRUE))

library(smoof)
library(emoa)
library(mco)
library(feather)
library(parallel)
library(MOEADps)

source("~/MOEADr/R/utils.R")
source("~/MOEADr/R/load.DTLZ.function.R")

repetitions <-  10
dimensions <- 40
lambda <- 50
pop.size <- 250

maxevals <- 100000
cores <-  16
cl <- makeCluster(cores)

decomp.small    <-
  list(name       = "sld", H = pop.size / 10 - 1)


decomp.big    <-
  list(name       = "sld", H = pop.size - 1)

variation = preset_moead("moead.de")$variation
variation[[2]]$pm = 1 / dimensions
scaling <- list()
scaling$name <- "simple"
neighbors.big <- preset_moead("moead.de")$neighbors
neighbors.big$T <- pop.size * 0.2
neighbors.small <- preset_moead("moead.de")$neighbors
neighbors.small$T <- (pop.size / 10) * 0.2
update <- preset_moead("moead.de")$update
update$UseArchive = TRUE

stopcrit  <- list(list(name    = "maxeval",
                       maxeval = maxevals))



n.obj <- 2

resource.allocation.1 <-
  list(
    name = "random",
    dt = 0,
    selection = "n",
    n = 1
  )

resource.allocation.small <-
  list(
    name = "random",
    dt = 0,
    selection = "n",
    n = (pop.size / 10) - n.obj
  )


problem.to.solve <-
  c("UF1",
    "UF2",
    "UF3",
    "UF4",
    "UF5",
    "UF6",
    "UF7",
    "UF8",
    "UF9",
    "UF10")
for (fun_name in problem.to.solve) {
  benchmark <- strsplit(fun_name, "[0-9]")[[1]][1]
  number <- as.integer(strsplit(fun_name, "[A-Z]")[[1]][3])
  problem.smoof.UF <-
    makeUFFunction(dimension = dimensions,
                   id = number)
  problem.UF <- function(X) {
    t(apply(X, MARGIN = 1,
            FUN = problem.smoof.UF))
  }
  if (number <= 7) {
    n.obj <- 2
  } else{
    n.obj <- 3
    
    # pop.size/10-1, = 24
    decomp.small    <-
      list(name       = "sld",
           H = 8,
           .nobj = 3) # 45
    
    
    decomp.big    <-
      list(name       = "sld", H = 21) # 253
    
    resource.allocation.small <-
      list(
        name = "random",
        dt = 0,
        selection = "n",
        n = 1 + n.obj
      )
    
    resource.allocation.big <-
      list(
        name = "random",
        dt = 0,
        selection = "n",
        n = 43
      )
  }
  par.set = ParamHelpers::getParamSet(problem.smoof.UF)
  fun <- list(
    name       = "problem.UF",
    xmin       = as.numeric(getLower(par.set)),
    xmax       = as.numeric(getUpper(par.set)),
    m          = n.obj
  )
  print(paste0(n.obj, " OBJECTIVES"))
  
  dir.name <- paste0("~/tec/",fun_name, "/")
  if (!dir.exists(dir.name))
    dir.create(dir.name)
  
  
  for (j in 1:repetitions) {
    cat("rep", j, "\n")
    
    seed <- sample(1:1000)[1]
    
    
    dir.name <- paste0("~/tec/",fun_name,"/moead_small_iter_",j,"/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)
    
    moead.small <- moeadps(
      problem  = fun,
      preset   = preset_moead("moead.de"),
      decomp = decomp.small,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors.small,
      showpars = list(show.iters = "none", showevery = 1000),
      seed = seed,
      update = update,
      saving.dir = dir.name
    )
    
    rm(moead.small)
    
    dir.name <- paste0("~/tec/",fun_name,"/moead_big_iter_",j,"/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)
    
    moead.big <- moeadps(
      problem  = fun,
      preset   = preset_moead("moead.de"),
      decomp = decomp.big,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors.big,
      showpars = list(show.iters = "none", showevery = 1000),
      seed = seed,
      update = update,
      saving.dir = dir.name
    )
    
    rm(moead.big)
    
    dir.name <- paste0("~/tec/",fun_name,"/moead_steady_iter_",j,"/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)
    
    moead.ps.1 <- moeadps(
      problem  = fun,
      preset   = preset_moead("moead.de"),
      decomp = decomp.big,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors.big,
      showpars = list(show.iters = "none", showevery = 1000),
      update = update,
      resource.allocation = resource.allocation.1,
      seed = seed,
      saving.dir = dir.name
    )
    rm(moead.ps.1)
    
    dir.name <- paste0("~/tec/",fun_name,"/moead_ps_25_iter_",j,"/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)
    
    moead.ps.small <- moeadps(
      problem  = fun,
      preset   = preset_moead("moead.de"),
      decomp = decomp.big,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors.big,
      showpars = list(show.iters = "none", showevery = 1000),
      update = update,
      resource.allocation = resource.allocation.small,
      seed = seed,
      saving.dir = dir.name
    )
    
    rm(moead.ps.small)
    
    
    
  }
  
  
}
