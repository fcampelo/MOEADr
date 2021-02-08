rm(list = ls(all = TRUE))

library(smoof)
library(emoa)
library(mco)
library(feather)
library(nsga2R)
library(parallel)
library(MOEADps)

source("~/MOEADr/R/utils.R")
source("~/MOEADr/R/load.DTLZ.function.R")

source('~/MOEADr/R/variation_de.R')
# source('~/MOEADr/R/moead.R')

source('~/MaOEA/R/maoea.R')
source('~/MaOEA/R/NSGA3.r') # based on MaOEA
source('~/MaOEA/R/functions.R')
source('~/MaOEA/R/nsga2.RA.R') # based on nsga2R
source('~/MaOEA/R/R2mtch.R')

cores <-  1
cl <- makeCluster(cores)

# which bbob function
instance <- 1
# number of objectives
n.obj <- 2
pop.size <- 500

maxevals <- 100000

repetitions <- 10

# decomp    <-
#   list(name       = "sld", H = pop.size-1, .nobj = n.obj)
# W <- decomposition_sld(decomp)

loaded.weights.2objs <-
  data.matrix(
    read.csv(
      "~/MOEADr/SOBOL-2objs-500wei.ws",
      header = F,
      stringsAsFactors = FALSE,
      sep = " "
    )
  )
W <- loaded.weights.2objs
decomp.loaded.2 <- list(name = "loaded", W = loaded.weights.2objs)



resource.allocation.25 <-
  list(
    name = "random",
    dt = 0,
    selection = "n",
    n = pop.size/10 
  )

variation = preset_moead("moead.de")$variation

scaling <- list()
scaling$name <- "simple"
neighbors.500 <- preset_moead("moead.de")$neighbors
neighbors.500$T <- pop.size* 0.2
update <- preset_moead("moead.de")$update
update$UseArchive = TRUE

stopcrit  <- list(list(name    = "maxeval",
                       maxeval = maxevals))



resource.allocation.nsga3 <-
  list(
    name = "random",
    dt = 0,
    selection = "n",
    n = pop.size/10 #got be pair value
  )

resource.allocation.nsga2 <-
  list(
    name = "random",
    dt = 0,
    selection = "n",
    n = pop.size/10 #got be pair value
  )

# ps_target <- 1 / (5 + ( 1 / 2  )^0.5)

nsga3RA <- data.frame()
nsga3 <- data.frame()
nsga2 <- data.frame()
nsga2RA <- data.frame()
MOEADPS <- data.frame()
MOEAD <- data.frame()

problem.to.solve <- c("BIBBOB55", "BIBBOB54", "BIBBOB53", "BIBBOB50", "BIBBOB47", "BIBBOB46", "BIBBOB41", "BIBBOB36", "BIBBOB35", "BIBBOB28", "BIBBOB21", "BIBBOB20", "BIBBOB11", "BIBBOB2", "BIBBOB1")

for (fun in problem.to.solve) {
  benchmark <- strsplit(fun, "[0-9]")[[1]][1]

  if(benchmark == "UF"){
    # dimensions
    dimensions <- 100
    number <- as.integer(strsplit(fun, "[A-Z]")[[1]][3])
    problem <-
      makeUFFunction(dimensions = dimensions, id = number)
  }
  else{
    # dimensions
    dimensions <- 40

    number <- as.integer(strsplit(fun, "[A-Z]")[[1]][7])
    problem <-
      makeBiObjBBOBFunction(dimensions = dimensions, fid = number, iid = instance)
  }
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
  # variation stack
  ctrl <- list(
    weightVector = t(W),
    mutationProbability = 1 / dimensions,
    mutationDistribution = 20,
    lower       = as.numeric(getLower(par.set)),
    upper       = as.numeric(getUpper(par.set))
  )
  variation[[2]]$pm = 1 / dimensions

  print(benchmark)
  print(number)
  
  dir.name <- paste0("~/tec/",fun, "/")
  if (!dir.exists(dir.name))
    dir.create(dir.name)

  for (i in 1:repetitions) {
    print(i)
    # random population definitions
    X  <- create_population(N       = pop.size,
                            problem = problem.bibbob)

    dir.name <- paste0("~/tec/",fun,"/nsga_3_iter_",i,"/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)
    nsga.3 <-
      optimMaOEA(
        x = t(X),
        fun = problem.bibbob,
        solver = NSGA3,
        nVar = dimensions,
        populationSize = pop.size,
        nObjective = n.obj,
        maxevals = maxevals,
        control = ctrl,
        saving.dir = dir.name
      )


    dir.name <- paste0("~/tec/",fun,"/nsga_3_ra_iter_",i,"/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)
    nsga.3.RA <-
      optimMaOEA(
        x = t(X),
        fun = problem.bibbob,
        solver = NSGA3,
        nVar = dimensions,
        populationSize = pop.size,
        nObjective = n.obj,
        maxevals = maxevals,
        control = ctrl,
        resource.allocation = resource.allocation.nsga3,
        saving.dir = dir.name
      )

    dir.name <- paste0("~/tec/",fun,"/moead_ra_iter_",i,"/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)
    moead.ps.50 <- moeadps(
      problem  = problem.bibbob,
      preset   = preset_moead("moead.de"),
      decomp = decomp.loaded.2,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors.500,
      showpars = list(show.iters = "none", showevery = 1000),
      update = update,
      resource.allocation = resource.allocation.25,
      saving.dir = dir.name
    )

    dir.name <- paste0("~/tec/",fun,"/moead_iter_",i,"/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)
    moead <- moeadps(
      problem  = problem.bibbob,
      preset   = preset_moead("moead.de"),
      decomp = decomp.loaded.2,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors.500,
      showpars = list(show.iters = "none", showevery = 1000),
      update = update,
      saving.dir = dir.name
    )

    dir.name <- paste0("~/tec/",fun,"/nsga_2_iter_",i,"/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)
    nsga.2 <-
      nsgaps(
        problem = problem.bibbob,
        varNo = dimensions,
        objDim = n.obj,
        lowerBounds = rep(as.numeric(getLower(par.set)), dimensions),
        upperBounds = rep(as.numeric(getUpper(par.set)), dimensions),
        popSize = pop.size,
        tourSize = 2,
        maxiter = maxevals,
        cprob = 0.9,
        XoverDistIdx = 20,
        mprob = 0.1,
        MuDistIdx = 3,
        parent = X,
        saving.dir = dir.name
      )
    # 
    dir.name <- paste0("~/tec/",fun,"/nsga_2_ra_iter_",i,"/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)

    nsga.2.ra <-
      nsgaps(
        problem = problem.bibbob,
        varNo = dimensions,
        objDim = n.obj,
        lowerBounds = rep(as.numeric(getLower(par.set)), dimensions),
        upperBounds = rep(as.numeric(getUpper(par.set)), dimensions),
        popSize = pop.size,
        tourSize = 2,
        maxiter = maxevals,
        cprob = 0.9,
        XoverDistIdx = 20,
        mprob = 0.1,
        MuDistIdx = 3,
        resource.allocation = resource.allocation.nsga2,
        parent = X,
        saving.dir = dir.name
      )


  }
}

stopCluster(cl)
