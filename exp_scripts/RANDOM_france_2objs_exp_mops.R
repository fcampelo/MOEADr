rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
library(feather)
library(withr)
library(compiler)
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)
enableJIT(1)

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
                       maxeval = 100000))



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

for (j in 1:repetitions) {
  number_subproblems <-
    c(3, 4, 5, 6, 7, 8, 9, 10, 30, 50, 100, 150, 250)
  cat("rep",j,"\n")
  for (lambda in number_subproblems) {
    cat("lambda", lambda, "\n")
    
    resource.allocation.RANDOM <-
      list(
        name = "random",
        dt = 0,
        selection = "n",
        n = lambda
      )
    
    moead.random <- moead(
      problem  = problem.dtlz7,
      preset   = preset_moead("moead.de"),
      decomp = decomp.loaded.2,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors,
      showpars = list(show.iters = "numbers", showevery = 100),
      seed = j,
      update = update,
      resource.allocation = resource.allocation.RANDOM,
      loaded.weights = loaded.weights.2objs
    )
    
    savePlotData(
      moea = moead.random,
      name = paste0(fun, "_moead.random_", lambda, "_"),
      j = j,
      wd = "~/france_data/"
    )
  }
  
  saving_number <- saving_number + 1
}
