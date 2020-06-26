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

repetitions <-  10
dimension <- 100

variation = preset_moead("moead.de")$variation
variation[[2]]$pm = 1 / dimension
scaling <- list()
scaling$name <- "simple"
neighbors <- preset_moead("moead.de")$neighbors
neighbors$T <- 100
update <- preset_moead("moead.de")$update
update$UseArchive = TRUE

stopcrit  <- list(list(name    = "maxeval",
                     maxeval = 10000000))
              
n.obj <- 3
print("3 OBJECTIVES")

fun = "UF9"

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
      problem  = problem.uf9,
      preset   = preset_moead("moead.de"),
      decomp = decomp.loaded.3,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors,
      showpars = list(show.iters = "numbers", showevery = 100),
      seed = j,
      update = update,
      resource.allocation = resource.allocation.RANDOM,
      loaded.weights = loaded.weights.3objs
    )
    
    savePlotData(
      moea = moead.random,
      name = paste0(fun, "_moead.random_", lambda, "_"),
      j = j,
      wd = "~/france_data/"
    )
    rm(moead.random)
  }
}
