rm(list = ls(all = TRUE))
# setwd("~/MOEADr/R/")
library(smoof)
# library(MOEADr)
library(emoa)
library(feather)
# # library(withr)
library(compiler)
# lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)
enableJIT(1)

library(MOEADps)

repetitions <-  10
dimension <- 100

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

n.obj <- 3
print("3 OBJECTIVES")

problem.to.solve <-
  c("UF7",
    "UF8",
    "UF9")

for (fun in problem.to.solve) {
  print(fun)
  benchmark <- strsplit(fun, "[0-9]")[[1]][1]
  number <- as.integer(strsplit(fun, "[A-Z]")[[1]][3])
  if (benchmark == "DTLZ")
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
  
  for (j in 1:repetitions) {
    number_subproblems <-
      c(4, 5, 6, 7, 8, 9, 10, 30, 50, 100, 150, 250)
    cat("rep", j, "\n")
    for (lambda in number_subproblems) {
      cat("lambda", lambda, "\n")
      
      seed <- sample(1:1000)[1]
      
      resource.allocation.RI <-
        list(
          name = "RI",
          dt = 1,
          selection = "n",
          n = lambda
        )
      
      dir.name <-
        paste0("~/france_data/",
               fun,
               "_moead.RI_",
               j,
               "/lambda_",
               lambda ,
               "/")
      if (!dir.exists(dir.name))
        dir.create(dir.name, recursive = T)
      e()
      moead.RI <- moeadps(
        problem  = problem.solving,
        preset   = preset_moead("moead.de"),
        decomp = decomp.loaded.3,
        variation = variation,
        stopcrit = stopcrit,
        scaling = scaling,
        neighbors = neighbors,
        showpars = list(show.iters = "none", showevery = 1000),
        update = update,
        resource.allocation = resource.allocation.RI,
        seed = seed,
        saving.dir = dir.name
      )
      
    }
  }
}