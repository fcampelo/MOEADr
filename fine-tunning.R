# setwd("~/MOEADr/")
suppressPackageStartupMessages(library(irace))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(smoof))
suppressPackageStartupMessages(library(MOEADr))

scenario                <- irace::defaultScenario()
scenario$seed           <- 123456 # Seed for the experiment
# scenario$targetRunner   <- "target.runner" # Runner function (def. below)
scenario$forbiddenFile  <- "../inst/extdata/forbidden.txt" # forbidden configs
scenario$debugLevel     <- 1
scenario$maxExperiments <- 2000 # Tuning budget
scenario$testNbElites   <- 7     # test all final elite configurations

# Number of cores to be used by irace (set with caution!)
nc                      <- parallel::detectCores() - 1
scenario$parallel       <- nc

# Read tunable parameter list from file
parameters <- readParameters("../inst/extdata/parameters.txt")

### Build training instances
fname   <- paste0("UF_", 1:1)
dims    <- c(20:29,
             # 31:39,
             # 41:49,
             51:60)

allfuns            <- expand.grid(fname, dims, stringsAsFactors = FALSE)
scenario$instances <- paste0(allfuns[,1], "_", allfuns[,2])

for (i in 1:nrow(allfuns)){
  assign(x     = scenario$instances[i],
         value = make_vectorized_smoof(prob.name  = "UF",
                                       dimensions = allfuns[i, 2],
                                       id         = as.numeric(strsplit(allfuns[i, 1], "_")
                                                               [[1]][2]))) }

### Build test instances
dims                   <- c(30)
allfuns                <- expand.grid(fname, dims, stringsAsFactors = FALSE)
scenario$testInstances <- paste0(allfuns[,1], "_", allfuns[,2])

for (i in 1:nrow(allfuns)){
  assign(x     = scenario$testInstances[i],
         value = make_vectorized_smoof(prob.name  = "UF",
                                       dimensions = allfuns[i, 2],
                                       id         = as.numeric(strsplit(allfuns[i, 1], "_")
                                                               [[1]][2]))) }

target.runner <- function(experiment, scenario){
  force(experiment)
  conf <- experiment$configuration
  inst <- experiment$instance

  # Assemble moead input lists
  ## 1. Problem
  fdef    <- unlist(strsplit(inst, split = "_"))
  uffun   <- smoof::makeUFFunction(dimensions = as.numeric(fdef[3]),
                                   id         = as.numeric(fdef[2]))
  fattr   <- attr(uffun, "par.set")
  problem <- list(name       = inst,
                  xmin       = fattr$pars$x$lower,
                  xmax       = fattr$pars$x$upper,
                  m          = attr(uffun, "n.objectives"))

  ## 2. Decomp
  decomp <- list(name = conf$decomp.name)
  if (problem$m == 2){ # <-- 2 objectives
    if(decomp$name == "SLD") decomp$H <- 99 # <-- yields N = 100
    if(decomp$name == "Uniform") decomp$N <- 100
  } else { # <-- 3 objectives
    if(decomp$name == "SLD") decomp$H <- 16 # <-- yields N = 153
    if(decomp$name == "Uniform") decomp$N <- 150
  }

  ## 3. Neighbors
  neighbors <- list(name    = conf$neighbor.name,
                    T       = conf$T,
                    delta.p = conf$delta.p)

  ## 4. Aggfun
  aggfun <- list(name = conf$aggfun.name)
  if (aggfun$name == "PBI") aggfun$theta <- conf$aggfun.theta

  ## 5. Update
  update <- list(name       = conf$update.name,
                 UseArchive = conf$UseArchive)
  if (update$name != "standard") update$nr <- conf$nr
  if (update$name == "best")     update$Tr <- conf$Tr

  ## 6. Scaling
  scaling <- list(name = "simple")

  ## 7. Constraint
  constraint<- list(name = "none")

  ## 8. Stop criterion
  stopcrit  <- list(list(name    = "maxeval",
                         maxeval = 100000))

  ## 9. Echoing
  showpars  <- list(show.iters = "none")

  ## 10. Variation stack
  variation <- list(list(name = conf$varop1),
                    list(name = conf$varop2),
                    list(name = conf$varop3),
                    list(name = conf$varop4),
                    list(name = "truncate"))

  for (i in seq_along(variation)){
    if (variation[[i]]$name == "binrec") {
      variation[[i]]$rho <- get(paste0("binrec.rho", i), conf)
    }
    if (variation[[i]]$name == "diffmut") {
      variation[[i]]$basis <- get(paste0("diffmut.basis", i), conf)
      variation[[i]]$Phi   <- NULL
    }
    if (variation[[i]]$name == "polymut") {
      variation[[i]]$etam <- get(paste0("polymut.eta", i), conf)
      variation[[i]]$pm   <- get(paste0("polymut.pm", i), conf)
    }
    if (variation[[i]]$name == "sbx") {
      variation[[i]]$etax <- get(paste0("sbx.eta", i), conf)
      variation[[i]]$pc   <- get(paste0("sbx.pc", i), conf)
    }
    if (variation[[i]]$name == "localsearch") {
      variation[[i]]$type     <- conf$ls.type
      variation[[i]]$gamma.ls <- conf$gamma.ls
    }
  }

  ## 11. Seed
  seed <- conf$seed

  # Run MOEA/D
  out <- moead(preset = NULL,
               problem, decomp,  aggfun, neighbors, variation, update,
               constraint, scaling, stopcrit, showpars, seed)

  # return IGD based on reference data
  Yref <- as.matrix(read.table(paste0("../inst/extdata/pf_data/",
                                      fdef[1], fdef[2], ".dat")))
  return(list(cost = calcIGD(Y = out$Y, Yref = Yref)))
}
scenario$targetRunner   <- target.runner
## Running the experiment
irace.output <- irace::irace(scenario, parameters)
saveRDS(irace.output, "../inst/extdata/RESULTS.rds")
file.copy(from = "irace.Rdata", to = "../inst/extdata/irace-tuning2.Rdata")


## Test returned configurations on test instances
testing.main(logFile = "../inst/extdata/irace-tuning.Rdata")
file.copy(from = "irace.Rdata", to = "../inst/extdata/irace-testing2.Rdata")
