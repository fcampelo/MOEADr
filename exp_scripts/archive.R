# rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
library(feather)
library(withr)
library(compiler)
library(ggplot2)
source("~/MOEADr/R/moead.R")
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)
enableJIT(1)

repetitions <-  10
dimension <- 100

decomp <- list(name = "sld", H = 2, .nobj = 2)
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
neighbors3$T <- 3
update <- preset_moead("moead.de")$update
update$UseArchive = TRUE

stopcrit  <- list(list(name    = "maxeval",
                       maxeval = 60000))



n.obj <- 2
# problem <-
#   load.UF.function(fun = "UF2", d = dimension)
problem <-
load.DTLZ.function("DTLZ3", dimension = dimension, n.obj = n.obj)
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

problem.to.solve <- c("DTLZ7", "DTLZ3", "UF2")
i <- 1
for (j in 4:6) {
  fun <- problem.to.solve[i]
  problem <-
    load.DTLZ.function(problem.to.solve[i], dimension = dimension, n.obj = n.obj)
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
  
  # number_subproblems <-
  #   c(3, 4, 5, 6, 7, 8, 9, 10, 30, 50, 100, 150, 250)
  number_subproblems <-
    c(3)
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
    
    
    moead.smallT <- moead(
      problem  = problem.dtlz7,
      preset   = preset_moead("moead.de"),
      decomp = decomp.loaded.2,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors,
      showpars = list(show.iters = "none", showevery = 1000),
      seed = j+42,
      update = update,
      resource.allocation = resource.allocation.RANDOM,
      small.update = TRUE
    )
    
    moead.random <- moead(
      problem  = problem.dtlz7,
      preset   = preset_moead("moead.de"),
      decomp = decomp.loaded.2,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors,
      showpars = list(show.iters = "none", showevery = 1000),
      seed = j+42,
      update = update,
      resource.allocation = resource.allocation.RANDOM
    )
    
    moead.3 <- moead(
      problem  = problem.dtlz7,
      preset   = preset_moead("moead.de"),
      decomp = decomp,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors3,
      showpars = list(show.iters = "none", showevery = 1000),
      seed = j+42,
      update = update
    )
    
    moead500 <- moead(
      problem  = problem.dtlz7,
      preset   = preset_moead("moead.de"),
      decomp = decomp.loaded.2,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors,
      showpars = list(show.iters = "none", showevery = 1000),
      seed = j+42,
      update = update
    )
    
  
  # exit()
    
    ecr::nondominated(t(moead.random$Archive$Y))
    
    ref.front <- rbind(moead.random$Archive$Y, moead.3$Archive$Y, moead500$Archive$Y, moead.smallT$Archive$Y)
    ref.point <- c(1,1)
    plot.data3 <-
      rbind(
        data.frame(moead.3$Archive$Y[ecr::nondominated(t(moead.3$Archive$Y)),], Strategy = "MOEA/D Pop = 3"),
        data.frame(moead500$Archive$Y[ecr::nondominated(t(moead500$Archive$Y)),], Strategy = "MOEA/D"),
        data.frame(moead.random$Archive$Y[ecr::nondominated(t(moead.random$Archive$Y)),], Strategy = "PS = 3"),
        data.frame(moead.smallT$Archive$Y[ecr::nondominated(t(moead.random$Archive$Y)),], Strategy = "MOEA/D T = 1")
      )
    print(ggplot(plot.data3, aes(f1, f2)) +
            # geom_line(aes(color = Strategy), alpha = 1) +
            geom_point(aes(color = Strategy, shape = Strategy), alpha = 1) + ggtitle(
      paste0(
        fun,
        ". HV (MOEAD Pop = 3): ",
        round(emoa::dominated_hypervolume(
          points = t(scaling_Y(moead.3$Archive$Y, ref.front)), ref = ref.point
        ), 2),
        ". HV (PS = 3): ",
        round(emoa::dominated_hypervolume(
          points = t(scaling_Y(moead.random$Archive$Y, ref.front)), ref = ref.point
        ), 2),
        ". HV of (MOEAD Pop = 500): ",
        round(emoa::dominated_hypervolume(
          points = t(scaling_Y(moead500$Archive$Y, ref.front)), ref = ref.point
        ), 2),
        ". HV of (MOEAD T.update = 1): ",
        round(emoa::dominated_hypervolume(
          points = t(scaling_Y(moead.smallT$Archive$Y, ref.front)), ref = ref.point
        ), 2)
      )
    ))
    
    filename = paste0("~/Desktop/", j, "_", fun , "_pf_archive.png")
    ggsave(filename = filename,
           dpi = 600,
           width = 9)
    
    
    i <- i + 1
    rm(moead.random)
    rm(moead500)
    rm(moead.smallT)
    rm(moead.3)
  }
}
