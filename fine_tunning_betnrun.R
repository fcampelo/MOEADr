# setwd("~/MOEADr/")
suppressPackageStartupMessages(library(irace))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(smoof))
suppressPackageStartupMessages(library(MOEADr))
library(R.utils)
setwd("~/MOEADr/R/")
sourceDirectory(".")
file.sources = list.files(pattern = "*.R")


scenario                <- irace::defaultScenario()
scenario$seed           <- 129446 # Seed for the experiment
scenario$debugLevel     <- 1
scenario$forbiddenFile  <- "../inst/extdata/forbidden2.txt" # forbidden configs
scenario$maxExperiments <- 40000 # Tuning budget
scenario$testNbElites   <- 7     # test all final elite configurations

# Number of cores to be used by irace (set with caution!)
nc                      <- (parallel::detectCores() - 1)
scenario$parallel       <- nc

# Read tunable parameter list from file
parameters <- readParameters("../inst/extdata/parameters2.txt")

### Build training instances
fname   <- paste0("UF_", 1:10)
dims    <- c(20:29,
             31:39,
             41:49,
             51:60)

allfuns            <-
  expand.grid(fname, dims, stringsAsFactors = FALSE)
scenario$instances <- paste0(allfuns[, 1], "_", allfuns[, 2])

for (i in 1:nrow(allfuns)) {
  assign(
    x     = scenario$instances[i],
    value = make_vectorized_smoof(
      prob.name  = "UF",
      dimensions = allfuns[i, 2],
      id         = as.numeric(strsplit(allfuns[i, 1], "_")
                              [[1]][2])
    )
  )
}

### Build test instances
dims                   <- c(30)
allfuns                <-
  expand.grid(fname, dims, stringsAsFactors = FALSE)
scenario$testInstances <- paste0(allfuns[, 1], "_", allfuns[, 2])

for (i in 1:nrow(allfuns)) {
  assign(
    x     = scenario$testInstances[i],
    value = make_vectorized_smoof(
      prob.name  = "UF",
      dimensions = allfuns[i, 2],
      id         = as.numeric(strsplit(allfuns[i, 1], "_")
                              [[1]][2])
    )
  )
}

target.runner <- function(experiment, scenario) {
  force(experiment)
  conf <- experiment$configuration
  inst <- experiment$instance
  # Assemble moead input lists
  ## 1. Problem
  fdef    <- unlist(strsplit(inst, split = "_"))
  uffun   <- smoof::makeUFFunction(dimensions = as.numeric(fdef[3]),
                                   id         = as.numeric(fdef[2]))
  fattr   <- attr(uffun, "par.set")
  problem <- list(
    name       = inst,
    xmin       = fattr$pars$x$lower,
    xmax       = fattr$pars$x$upper,
    m          = attr(uffun, "n.objectives")
  )

  ## 2. Decomp
  # decomp <- list(name = conf$decomp.name)
  decomp <- list(name = "SLD")
  if (problem$m == 2) {
    # <-- 2 objectives
    decomp$H <- 199
  } else {
    # <-- 3 objectives
    decomp$H <- 19
  }

  ## 8. Stop criterion
  maxiter <- 300

  ## 11. Seed
  seed <- conf$seed

  ## 5. bet-and-run
  betnrun.conf <- list(
    problem  = problem,
    preset   = preset_moead("moead.de"),
    decomp = decomp,
    showpars = list(show.iters = "none", showevery = 100),
    stopcrit = NULL,
    seed = NULL
  )
  betnrun <- restart.moead(
    algo_conf = betnrun.conf,
    num.iter.run = maxiter,
    seed = sample(1:10000, 1),
    number.instances = conf$number.instances,
    num.iter.bet = conf$num.iter.bet,
    algo = "moead.de"
  )
  # return HV
  ref.points <-
    rep(10, problem$m)

  betnrun.hv <- emoa::dominated_hypervolume(points = t(betnrun$Y), ref.points)
  return(list(cost = (-1) * betnrun.hv))
}
scenario$targetRunner   <- target.runner
## Running the experiment
irace.output <- irace::irace(scenario, parameters)
setwd("~/MOEADr/R/")
saveRDS(irace.output, "../inst/extdata/RESULTS2.rds")
setwd("~/MOEADr/R/")
file.copy(from = "irace.Rdata", to = "../inst/extdata/irace-tuning2.Rdata")

# ## Test returned configurations on test instances
setwd("~/MOEADr/R/")
testing.main(logFile = "../inst/extdata/irace-tuning2.Rdata")
setwd("~/MOEADr/R/")
file.copy(from = "irace.Rdata", to = "../inst/extdata/irace-testing2.Rdata")


load("../inst/extdata/irace-testing2.Rdata")
indx <- iraceResults$allElites[[length(iraceResults$allElites)]]
finalConfs <- iraceResults$allConfigurations[indx,]

### change internal structure of "parameters" to allow using function
### irace::parameterFrequency()
mypars <- iraceResults$parameters
mypars$names      <- c("number.instances" ,          "num.iter.bet")
mypars$nbVariable <- 2

### Plot
irace::parameterFrequency(finalConfs, mypars, cols = 2)
irace::getFinalElites(iraceResults, n=1)
