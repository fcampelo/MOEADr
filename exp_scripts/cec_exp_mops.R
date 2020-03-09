rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
library(feather)
library(withr)
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)

num_pf <- 11
begin <- 10

repetitions <-  21
dimension <- 100 # dimenstion <= 40 when using BiBBOB
algorithms <- c("moead.de")
variation = preset_moead("moead.de")$variation
variation[[2]]$pm = 1 / dimension
scaling <- list()
scaling$name <- "simple"
subproblems.number <-
  c(349, 25) #yelds, with sld, to 350 and 351 solutions in 2 and 3 objectives

### I WAS NOT USING ARCHIVE, IM GOING TO DO AFTER CEC
# update <- preset_moead("moead.de")$update
# update$UseArchive = TRUE
stopcrit  <- list(list(name    = "maxeval",
                       maxeval = 30000))
# stopcrit  <- list(list(name    = "maxiter",
#                        maxiter = 2))



resource.allocation.RANDOM_FIXED.0.1 <-
  list(
    name = "random_fixed",
    dt = 20,
    # heads up
    selection = "random",
    # in case we want to move from stochastic
    fixed_value = 0.1
  )
resource.allocation.RANDOM_FIXED.0.2 <-
  list(
    name = "random_fixed",
    dt = 20,
    # heads up
    selection = "random",
    # in case we want to move from stochastic
    fixed_value = 0.2
  )
resource.allocation.RANDOM_FIXED.0.4 <-
  list(
    name = "random_fixed",
    dt = 20,
    # heads up
    selection = "random",
    # in case we want to move from stochastic
    fixed_value = 0.4
  )
resource.allocation.RANDOM_FIXED.0.6 <-
  list(
    name = "random_fixed",
    dt = 20,
    selection = "random",
    # in case we want to move from stochastic
    fixed_value = 0.6
  )
resource.allocation.RANDOM_FIXED.0.8 <-
  list(
    name = "random_fixed",
    dt = 20,
    # heads up
    selection = "random",
    # in case we want to move from stochastic
    fixed_value = 0.8
  )
resource.allocation.RANDOM_FIXED.1.0 <-
  list(
    name = "random_fixed",
    dt = 20,
    # heads up
    selection = "random",
    # in case we want to move from stochastic
    fixed_value = 1.0
  )
resource.allocation.NORM <-
  list(name = "norm",
       dt = 20,
       selection = "random")

resource.allocation.NORM.inverse <-
  list(name = "inverse",
       dt = 20,
       selection = "random")

resource.allocation.RI <-
  list(name = "RI",
       dt = 20,
       # heads up, necessary here
       selection = "random") # in case we want to move from stochastic)
resource.allocation.MRDL <-
  list(name = "rad",
       dt = 20,
       selection = "random")

resource.allocation.MRDL.inverse <-
  list(name = "rad_inverse",
       dt = 20,
       selection = "random")



fun.names1 <- list()
algo <- "moead.de"
for (i in 1:10) {
  fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
}

for (i in 1:7) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}
##### NOT IN THIS EXP
# for (i in c(20,21,22,23,26,27,28,29,30,33,34,35,36,39,40,41,44,45,53,54,55)) {
#   fun.names1[[length(fun.names1) + 1]] = paste0("BiObjBBOB", i)
# }

for (fun in fun.names1) {
  benchmark <- strsplit(fun, "[0-9]")[[1]][1]
  number <- strsplit(fun, "[A-Z]")[[1]]
  number <- number[length(number)]
  if (benchmark == "DTLZ") {
    decomp <- list(name = "SLD", H = subproblems.number[1])
    n.obj <- 2
    problem <-
      load.DTLZ.function(fun, dimension = dimension, n.obj = n.obj)
    problem.smoof <- problem$fn
    problem.DTLZ <- function(X) {
      t(apply(X, MARGIN = 1,
              FUN = problem.smoof))
    }
    par.set = ParamHelpers::getParamSet(problem.smoof)
    problem <- list(
      name       = "problem.DTLZ",
      xmin       = as.numeric(getLower(par.set)),
      xmax       = as.numeric(getUpper(par.set)),
      m          = n.obj
    )
  }
  else  if (benchmark == "UF") {
    if (as.numeric(number) == 8 ||
        as.numeric(number) == 9 ||
        as.numeric(number) == 10) {
      decomp <- list(name = "SLD", H = subproblems.number[2])
      n.obj <- 3
    }
    else{
      decomp <- list(name = "SLD", H = subproblems.number[1])
      n.obj <- 2
    }
    problem.smoof <-
      makeUFFunction(dimension = dimension,
                     id = as.numeric(number))
    problem.UF <- function(X) {
      t(apply(X, MARGIN = 1,
              FUN = problem.smoof))
    }
    par.set = ParamHelpers::getParamSet(problem.smoof)
    problem <- list(
      name       = "problem.UF",
      xmin       = as.numeric(getLower(par.set)),
      xmax       = as.numeric(getUpper(par.set)),
      m          = n.obj
    )
  }
  else  if (benchmark == "BiObjBBOB") {
    decomp <- list(name = "SLD", H = subproblems.number[1])
    n.obj <- 2
    problem.smoof <-
      smoof::makeBiObjBBOBFunction(dimension = 40,
                                   fid = as.integer(number),
                                   iid = 1) # right now I dont consider any instance
    problem.BiBBOB <- function(X) {
      t(apply(X, MARGIN = 1,
              FUN = problem.smoof))
    }
    par.set = ParamHelpers::getParamSet(problem.smoof)
    problem <- list(
      name       = "problem.BiBBOB",
      xmin       = as.numeric(getLower(par.set)),
      xmax       = as.numeric(getUpper(par.set)),
      m          = n.obj
    )
  }
  
  
  
  
  j <- 1
  while (j <= repetitions) {
    i <- 1
    # sampled <- sample(1:num_pf, num_pf)
    sampled <- sample(begin:num_pf, (num_pf+1)-begin)
    while (i <= num_pf) {
      print("iter")
      cat(i, sampled[i])
      if (sampled[i] == 1) {
        moead.random_fixed.0.1 <- moead(
          problem  = problem,
          preset   = preset_moead(algo),
          decomp = decomp,
          variation = variation,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j + 8753,
          resource.allocation = resource.allocation.RANDOM_FIXED.0.1
        )
        savePlotData(
          moea = moead.random_fixed.0.1,
          name = paste0(fun, "moead.random_fixed.0.1"),
          j = j
        )
      }
      else if (sampled[i] == 2) {
        moead.random_fixed.0.2 <- moead(
          problem  = problem,
          preset   = preset_moead(algo),
          decomp = decomp,
          variation = variation,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j + 8753,
          resource.allocation = resource.allocation.RANDOM_FIXED.0.2
        )
        savePlotData(
          moea = moead.random_fixed.0.2,
          name = paste0(fun, "moead.random_fixed.0.2"),
          j = j
        )
      }
      else if (sampled[i] == 3) {
        moead.random_fixed.0.4 <- moead(
          problem  = problem,
          preset   = preset_moead(algo),
          decomp = decomp,
          variation = variation,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j + 8753,
          resource.allocation = resource.allocation.RANDOM_FIXED.0.4
        )
        savePlotData(
          moea = moead.random_fixed.0.4,
          name = paste0(fun, "moead.random_fixed.0.4"),
          j = j
        )
      }
      else if (sampled[i] == 4) {
        moead.random_fixed.0.6 <- moead(
          problem  = problem,
          preset   = preset_moead(algo),
          decomp = decomp,
          variation = variation,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j + 8753,
          resource.allocation = resource.allocation.RANDOM_FIXED.0.6
        )
        savePlotData(
          moea = moead.random_fixed.0.6,
          name = paste0(fun, "moead.random_fixed.0.6"),
          j = j
        )
      }
      else if (sampled[i] == 5) {
        moead.random_fixed.0.8 <- moead(
          problem  = problem,
          preset   = preset_moead(algo),
          decomp = decomp,
          variation = variation,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j + 8753,
          resource.allocation = resource.allocation.RANDOM_FIXED.0.8
        )
        savePlotData(
          moea = moead.random_fixed.0.8,
          name = paste0(fun, "moead.random_fixed.0.8"),
          j = j
        )
      }
      else if (sampled[i] == 6) {
        moead.de <- moead(
          problem  = problem,
          preset   = preset_moead(algo),
          decomp = decomp,
          variation = variation,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j
        )
        savePlotData(moea = moead.de,
                     name = paste0(fun, "moead.de"),
                     j = j)
      }
      else if (sampled[i] == 7) {
        moead.de.RI <- moead(
          problem  = problem,
          preset   = preset_moead(algo),
          decomp = decomp,
          variation = variation,
          stopcrit = stopcrit,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j + 8753,
          resource.allocation = resource.allocation.RI
        )
        savePlotData(
          moea = moead.de.RI,
          name = paste0(fun, "moead.de.RI"),
          j = j
        )
      }
      else if (sampled[i] == 8) {
        moead.norm <- moead(
          problem  = problem,
          preset   = preset_moead(algo),
          decomp = decomp,
          variation = variation,
          stopcrit = stopcrit,
          scaling = scaling,
          # update = update,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j + 8753,
          resource.allocation = resource.allocation.NORM
        )
        savePlotData(
          moea = moead.norm,
          name = paste0(fun, "moead.norm"),
          j = j
        )
      }
      else if (sampled[i] == 9) {
        moead.norm.inverse <- moead(
          problem  = problem,
          preset   = preset_moead(algo),
          decomp = decomp,
          variation = variation,
          stopcrit = stopcrit,
          scaling = scaling,
          # update = update,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j + 8753,
          resource.allocation = resource.allocation.NORM.inverse
        )
        savePlotData(
          moea = moead.norm.inverse,
          name = paste0(fun, "moead.norm.inverse"),
          j = j
        )
      }
      else if (sampled[i] == 10) {
        moead.MRDL <- moead(
          problem  = problem,
          preset   = preset_moead(algo),
          decomp = decomp,
          variation = variation,
          stopcrit = stopcrit,
          scaling = scaling,
          # update = update,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j + 8753,
          resource.allocation = resource.allocation.MRDL
        )
        savePlotData(
          moea = moead.MRDL,
          name = paste0(fun, "moead.MRDL"),
          j = j
        )
      }
      else if (sampled[i] == 11) {
        moead.MRDL.inverse <- moead(
          problem  = problem,
          preset   = preset_moead(algo),
          decomp = decomp,
          variation = variation,
          stopcrit = stopcrit,
          scaling = scaling,
          # update = update,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j + 8753,
          resource.allocation = resource.allocation.MRDL.inverse
        )
        savePlotData(
          moea = moead.MRDL.inverse,
          name = paste0(fun, "moead.MRDL.inverse"),
          j = j
        )
      }
      
      
      i <- i + 1
    }
    j <- j + 1
    eixt()
  }
}