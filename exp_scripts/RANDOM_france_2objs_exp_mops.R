rm(list = ls(all = TRUE))

library(parallel)
library(smoof)
library(emoa)
library(feather)
library(compiler)
enableJIT(1)
source("~/MOEADr/R/load.DTLZ.function.R")
library(MOEADps)

cores <-  2
cl <- makeCluster(cores)

repetitions <-  10
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
                       maxeval = 100000))



n.obj <- 2
problem.to.solve <-
  c(
    "DTLZ1",
    "DTLZ2",
    "DTLZ3",
    "DTLZ4",
    "DTLZ5",
    "DTLZ6",
    "UF1",
    "UF2",
    "UF3",
    "UF4",
    "UF5",
    "UF7",
    "UF6"
  )
for (fun in problem.to.solve) {
  print(fun)
  benchmark <- strsplit(fun, "[0-9]")[[1]][1]
  number <- as.integer(strsplit(fun, "[A-Z]")[[1]][3])
  if(benchmark == "DTLZ")
  {
    problem <-
      load.DTLZ.function(fun, dimensions = dimension, n.obj = n.obj)
    problem.smoof.DTLZ <- problem$fn
    problem.DTLZ <- function(X) {
      t(apply(X, MARGIN = 1,
              FUN = problem.smoof.DTLZ))
    }
    par.set = ParamHelpers::getParamSet(problem.smoof.DTLZ)
    problem.dtlzX <- list(
      name       = "problem.DTLZ",
      xmin       = as.numeric(getLower(par.set)),
      xmax       = as.numeric(getUpper(par.set)),
      m          = n.obj
    )
    problem.solving <- problem.dtlzX
  } 
  else{
    problem.smoof.UF <-
      makeUFFunction(dimensions = dimension,
                     id = number)
    problem.UF <- function(X) {
      t(apply(X, MARGIN = 1,
              FUN = problem.smoof.UF))
    }
    
    par.set = ParamHelpers::getParamSet(problem.smoof.UF)
    problem.ufX <- list(
      name       = "problem.UF",
      xmin       = as.numeric(getLower(par.set)),
      xmax       = as.numeric(getUpper(par.set)),
      m          = n.obj
    )
    problem.solving <- problem.ufX
  }
  
  
  print("2 OBJECTIVES")
  e()
  for (j in 1:repetitions) {
    number_subproblems <-
      c(3, 4, 6, 8, 10, 30, 50, 100, 150, 250)
    cat("rep", j, "\n")
    for (lambda in number_subproblems) {
      cat("lambda", lambda, "\n")
      
      seed <- sample(1:1000)[1]
      
      resource.allocation.RANDOM <-
        list(
          name = "random",
          dt = 0,
          selection = "n",
          n = lambda
        )
      
      dir.name <-
        paste0("~/france_data/",
               fun,
               "_moead.random_",
               j,
               "/lambda_",
               lambda ,
               "/")
      if (!dir.exists(dir.name))
        dir.create(dir.name, recursive = T)
      
      moead.random <- moeadps(
        problem  = problem.solving,
        preset   = preset_moead("moead.de"),
        decomp = decomp.loaded.2,
        variation = variation,
        stopcrit = stopcrit,
        scaling = scaling,
        neighbors = neighbors,
        showpars = list(show.iters = "none", showevery = 1000),
        update = update,
        resource.allocation = resource.allocation.RANDOM,
        seed = seed,
        saving.dir = dir.name
      )
      
      
    }
  }
}

stopCluster(cl)