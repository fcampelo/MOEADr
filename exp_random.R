rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
library(feather)
library(withr)
source("~/myMOEADr/load.DTLZ.function.R")
source("~/myMOEADr/load.DTLZ.pareto.front.R")
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)


repetitions <-  21
dimension <- 100
algorithms <- c("moead.de")


resource.allocation.RANDOM_FIXED.0.1 <-
    list(
        name = "random_fixed",
        dt = 20,
        selection = "random",
        fixed_value = 0.1
    )
resource.allocation.RANDOM_FIXED.0.2 <-
    list(
        name = "random_fixed",
        dt = 20,
        selection = "random",
        fixed_value = 0.2
    )
resource.allocation.RANDOM_FIXED.0.4 <-
    list(
        name = "random_fixed",
        dt = 20,
        selection = "random",
        fixed_value = 0.4
    )
resource.allocation.RANDOM_FIXED.0.6 <-
    list(
        name = "random_fixed",
        dt = 20,
        selection = "random",
        fixed_value = 0.6
    )
resource.allocation.RANDOM_FIXED.0.8 <-
    list(
        name = "random_fixed",
        dt = 20,
        selection = "random",
        fixed_value = 0.8
    )
resource.allocation.RANDOM_FIXED.1.0 <-
    list(
        name = "random_fixed",
        dt = 20,
        selection = "random",
        fixed_value = 1.0
    )


variation = preset_moead("moead.de")$variation
variation[[2]]$pm = 1 / dimension
scaling <- list()
scaling$name <- "simple"
stopcrit  <- list(list(name    = "maxeval",
                       maxeval = 30000))


fun.names1 <- list()
algo <- "moead.de"
#for (i in 1:10) {
#    fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
#}

for (i in 1:7) {
    fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}


my.data <- data.frame()

for (fun in fun.names1) {
    benchmark <- strsplit(fun, "[0-9]")[[1]][1]
    number <- strsplit(fun, "[A-Z]")[[1]][3]
    if(benchmark == "DTLZ") number <- -1
    if (as.numeric(number) == 8 ||
        as.numeric(number) == 9 || as.numeric(number) == 10) {
        decomp <- list(name = "SLD", H = 25)
        n.obj <- 3
    }
    else{
        decomp <- list(name = "SLD", H = 349)
        n.obj <- 2
    }
    print(fun)
    #print(benchmark)
    if (benchmark == "DTLZ") {
	#print("eyah")     
   problem <-
            load.DTLZ.function(fun, dimension = dimension, n.obj = n.obj)
        problem <- problem$fn
        problem.DTLZ <- function(X) {
            t(apply(X, MARGIN = 1,
                    FUN = problem))
        }
        
        par.set = ParamHelpers::getParamSet(problem)
        problem.zdt1 <- list(
            name       = "problem.DTLZ",
            xmin       = as.numeric(getLower(par.set)),
            xmax       = as.numeric(getUpper(par.set)),
            m          = n.obj
       )
   }
    else{
	
        problem <-
            makeUFFunction(dimension = dimension,
                           id = as.numeric(number))
        problem.UF <- function(X) {
            t(apply(X, MARGIN = 1,
                    FUN = problem))
            
        }
    
    
    
    par.set = ParamHelpers::getParamSet(problem)
    problem.zdt1 <- list(
        name       = "problem.UF",
        xmin       = as.numeric(getLower(par.set)),
        xmax       = as.numeric(getUpper(par.set)),
        m          = n.obj
    )
    }
    j <- 1
    while (j <= repetitions) {
        i = 1
        sampled <- sample(1:8, 8)
        while (i <= 8) {
            if (sampled[i] == 2) {
                moead.random_fixed.0.1 <- moead(
                    problem  = problem.zdt1,
                    preset   = preset_moead(algo),
                    decomp = decomp,
                    variation = variation,
                    stopcrit = stopcrit,
                    scaling = scaling,
                    showpars = list(show.iters = "none", showevery = 10),
                    seed = j + 123,
                    resource.allocation = resource.allocation.RANDOM_FIXED.0.1
                )
                savePlotData(
                    moea = moead.random_fixed.0.1,
                    name = paste0(fun, "moead.random_fixed.0.1"),
                    j = j
                )
            }
            else if (sampled[i] == 4) {
                moead.random_fixed.0.2 <- moead(
                    problem  = problem.zdt1,
                    preset   = preset_moead(algo),
                    decomp = decomp,
                    variation = variation,
                    stopcrit = stopcrit,
                    scaling = scaling,
                    showpars = list(show.iters = "none", showevery = 10),
                    seed = j + 123,
                    resource.allocation = resource.allocation.RANDOM_FIXED.0.2
                )
                savePlotData(
                    moea = moead.random_fixed.0.2,
                    name = paste0(fun, "moead.random_fixed.0.2"),
                    j = j
                )
            }
            else if (sampled[i] == 5) {
                moead.random_fixed.0.4 <- moead(
                    problem  = problem.zdt1,
                    preset   = preset_moead(algo),
                    decomp = decomp,
                    variation = variation,
                    stopcrit = stopcrit,
                    scaling = scaling,
                    showpars = list(show.iters = "none", showevery = 10),
                    seed = j + 123,
                    resource.allocation = resource.allocation.RANDOM_FIXED.0.4
                )
                savePlotData(
                    moea = moead.random_fixed.0.4,
                    name = paste0(fun, "moead.random_fixed.0.4"),
                    j = j
                )
            }
            else if (sampled[i] == 6) {
                moead.random_fixed.0.6 <- moead(
                    problem  = problem.zdt1,
                    preset   = preset_moead(algo),
                    decomp = decomp,
                    variation = variation,
                    stopcrit = stopcrit,
                    scaling = scaling,
                    showpars = list(show.iters = "none", showevery = 10),
                    seed = j + 123,
                    resource.allocation = resource.allocation.RANDOM_FIXED.0.6
                )
                savePlotData(
                    moea = moead.random_fixed.0.6,
                    name = paste0(fun, "moead.random_fixed.0.6"),
                    j = j
                )
            }
            else if (sampled[i] == 7) {
                moead.random_fixed.0.8 <- moead(
                    problem  = problem.zdt1,
                    preset   = preset_moead(algo),
                    decomp = decomp,
                    variation = variation,
                    stopcrit = stopcrit,
                    scaling = scaling,
                    showpars = list(show.iters = "none", showevery = 10),
                    seed = j + 123,
                    resource.allocation = resource.allocation.RANDOM_FIXED.0.8
                )
                savePlotData(
                    moea = moead.random_fixed.0.8,
                    name = paste0(fun, "moead.random_fixed.0.8"),
                    j = j
                )
            }
            else if (sampled[i] == 8) {
                moead.random_fixed.1.0 <- moead(
                    problem  = problem.zdt1,
                    preset   = preset_moead(algo),
                    decomp = decomp,
                    variation = variation,
                    stopcrit = stopcrit,
                    scaling = scaling,
                    showpars = list(show.iters = "none", showevery = 10),
                    seed = j + 123,
                    resource.allocation = resource.allocation.RANDOM_FIXED.1.0
                )
                savePlotData(
                    moea = moead.random_fixed.1.0,
                    name = paste0(fun, "moead.random_fixed.1.0"),
                    j = j
                )
            } else if (sampled[i] == 8) {
                moead.random_fixed.1.0 <- moead(
                    problem  = problem.zdt1,
                    preset   = preset_moead(algo),
                    decomp = decomp,
                    variation = variation,
                    stopcrit = stopcrit,
                    scaling = scaling,
                    showpars = list(show.iters = "none", showevery = 10),
                    seed = j + 123,
                    resource.allocation = resource.allocation.RANDOM_FIXED.1.0
                )
                savePlotData(
                    moea = moead.random_fixed.1.0,
                    name = paste0(fun, "moead.random_fixed.1.0"),
                    j = j
                )
            }
            
            
            i <- i + 1
        }
        j <- j + 1
    }
}
