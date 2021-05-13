rm(list = ls(all = TRUE))

library(smoof)
library(MOEADps)
library(feather)
source('~/MOEADr/R/linPF.R')
source("~/MOEADr/R/utils.R")
source("~/MOEADr/R/moead.R")

repetitions <-  10
dimensions <- 40


loaded.weights.500 <-
  data.matrix(
    read.csv(
      "~/MOEADr/weights-sobol/SOBOL-3objs-500wei.ws",
      header = F,
      stringsAsFactors = FALSE,
      sep = " "
    )
  )
decomp500 <- list(name = "loaded", W = loaded.weights.500)

## 2 - Neighbors
neighbors <- preset_moead("moead.de")$neighbors
neighbors$T <- 300 * 0.2

## 3 - Aggregation function
aggfun <- list(name = "wt")

## 4 - Update strategy
update <- preset_moead("moead.de")$update
update$UseArchive = TRUE

## 5 - Scaling
scaling <- list(name = "simple")

## 6 - Stop criterion
stopcrit  <- list(list(name  = "maxeval",
                       maxeval  = 30000))

## 7 - Variation Operators
variation <- preset_moead("moead.de")$variation
variation[[2]]$pm = 1 / 12#dimension

resource.allocation.RANDOM <-
  list(
    name = "random",
    dt = 0,
    selection = "n",
    n = 500 / 10 - 3,
    period = -1
  )

tmp <-
  makeLINPF1Function()

problem.list <- function(X) {
  t(apply(X, MARGIN = 1,
          FUN = tmp))
}

par.set = ParamHelpers::getParamSet(problem)
problem.solving <- list(
  name       = "problem.list",
  xmin = as.numeric(getLower(par.set)),
  xmax = as.numeric(getUpper(par.set)),
  m    = 3
)


# X <- create_population(20, problem.solving)
# Y <- evaluate_population(X, problem.solving, nfe = 0)


for (i in 1:repetitions) {
  moead.random <- MOEADps::moeadps(
    problem  = problem.solving,
    decomp = decomp500,
    neighbors = neighbors,
    aggfun = aggfun,
    scaling = scaling,
    stopcrit = stopcrit,
    update = update,
    variation = variation,
    resource.allocation = resource.allocation.RANDOM,
    # saving.dir = paste0("~/Desktop/linPF/random/", i, "_")
  )
  
  # moead <- moeadps(
  #   preset   = preset_moead("moead.de"),
  #   problem  = problem.solving,
  #   decomp = decomp500,
  #   neighbors = neighbors,
  #   aggfun = aggfun,
  #   scaling = scaling,
  #   stopcrit = stopcrit,
  #   update = update,
  #   variation = variation,
  #   saving.dir = paste0("~/Desktop/linPF/moead/", i, "_")
  # )
}
# ref1 <- rbind(moead.random$Y, moead$Y)
# moead.random$scaled <- scaling_Y(moead.random$Y, ref1)
# moead$scaled <- scaling_Y(moead$Y, ref1)
#
# ref.point <- c(1.1,1.1,1.1)
# emoa::dominated_hypervolume(t(moead.random$scaled), ref = ref.point)
# emoa::dominated_hypervolume(t(moead$scaled), ref = ref.point)
#
#
#
#
