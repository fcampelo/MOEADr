setwd("~/MOEADr/R")
suppressPackageStartupMessages(library(irace))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(smoof))
# suppressPackageStartupMessages(library(MOEADr))
library(emoa)
library(ecr)
library(stringr)
library(withr)
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)
scenario                <- irace::defaultScenario()
scenario$seed           <- 302 # Seed for the experiment
scenario$targetRunner   <-
  "target.runner" # Runner function (def. below)
scenario$forbiddenFile  <-
  "../inst/extdata/forbidden.txt" # forbidden configs
scenario$debugLevel     <- 1
scenario$maxExperiments <- 20000 # Tuning budget
scenario$testNbElites   <-
  7     # test all final elite configurations

# Number of cores to be used by irace (set with caution!)
nc                      <- parallel::detectCores() - 5
scenario$parallel       <- nc

# Read tunable parameter list from file
parameters <- readParameters("../inst/extdata/parameters4.txt")

### Build training instances
fname   <- paste0("UF_", 1:1)
dims    <- c(20:29)#,
# 31:39,#
# # 41:49,
# 51:60)

# allfuns            <- expand.grid(fname, dims, stringsAsFactors = FALSE)
# scenario$instances <- paste0(allfuns[,1], "_", allfuns[,2])
scenario$instances <- paste0(1)


target.runner <- function(experiment, scenario) {
  force(experiment)
  conf <- experiment$configuration
  inst <- experiment$instance
  
  # Assemble moead input lists
  ## 1. Problem
  problem <- list(
    name       = "problem.moon",
    xmin       = rep(0, 2),
    xmax       = rep(1, 2),
    m          = 3,
    constraints = list(name      = "box_constraints", # constraint function routine
                       epsilon   = 0.00) # tolerance for equality constraints
  )
  
  
  ## 2. Decomp
  decomp <- list(name = "SLD")
  if (problem$m == 2) {
    # <-- 2 objectives
    if (decomp$name == "SLD")
      decomp$H <- 99 # <-- yields N = 100
    if (decomp$name == "Uniform")
      decomp$N <- 100
  } else {
    # <-- 3 objectives
    if (decomp$name == "SLD")
      decomp$H <- 16 # <-- yields N = 153
    if (decomp$name == "Uniform")
      decomp$N <- 150
  }
  
  ## 3. Neighbors
  neighbors <- list(name    = "lambda",
                    T       = conf$T,
                    delta.p = conf$delta.p)
  write.table(paste(conf$T), "ifnops.txt")
  
  ## 4. Aggfun
  aggfun <- list(name = conf$aggfun.name)
  if (aggfun$name == "PBI")
    aggfun$theta <- conf$aggfun.theta
  
  ## 5. Update
  update <- list(name       = conf$update.name,
                 UseArchive = conf$UseArchive)
  if (update$name != "standard")
    update$nr <- conf$nr
  if (update$name == "best")
    update$Tr <- conf$Tr
  
  ## 6. Scaling
  scaling <- list(name = "none")
  
  ## 7. Constraint
  constraint = list(name = "penalty", beta = get(paste0("beta"), conf))
  
  ## 8. Stop criterion
  stopcrit  <- list(list(name    = "maxeval",
                         maxeval = 30000))
  
  ## 9. Echoing
  showpars  <- list(show.iters = "none")
  
  variation <- preset_moead("moead.de")$variation
  # variation[[4]] <- variation[[3]]
  variation[[1]] <-
    list(name = "de")
  
  for (i in seq_along(variation)) {
    if (variation[[i]]$name == "polymut") {
      variation[[i]]$etam <- get(paste0("polymut.eta"), conf)
      variation[[i]]$pm   <- get(paste0("polymut.pm"), conf)
    }
  }
  
  ## 11. Seed
  seed <- sample(1:9999, 1)
  # seed <- conf$seed
  
  # Run MOEA/D
  my.file.n <- paste0("tuning/", with_options(c(scipen = 999),
                                              str_pad(seed, 3, pad = "0")))
  system(paste("mkdir ", paste0(my.file.n, "th_run")))
  system(paste("mkdir ", paste0(my.file.n, "th_run/optimizer")))
  system(paste("mkdir ", paste0(
    my.file.n, "th_run/optimizer/interface"
  )))
  
  out <- moead(
    preset = preset_moead("moead.de"),
    problem,
    decomp,
    aggfun,
    neighbors,
    variation,
    update,
    scaling,
    constraint,
    stopcrit,
    showpars = list(show.iters = "none", showevery = 100),
    seed = seed,
    my.file.n = my.file.n
  )
  
  ref.points <- matrix(c(1.0, 0.0, 1.0), nrow = 1, ncol = 3)
  
  hv <-
    emoa::dominated_hypervolume(points = t(out$Y),
                                ref = ref.points)
  
  return(list(cost = (-1) * hv))
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
finalConfs <- iraceResults$allConfigurations[indx, ]

### change internal structure of "parameters" to allow using function
### irace::parameterFrequency()
mypars <- iraceResults$parameters
mypars$names      <- c("number.instances" ,          "num.iter.bet")
mypars$nbVariable <- 2

### Plot
irace::parameterFrequency(finalConfs, mypars, cols = 2)
irace::getFinalElites(iraceResults, n = 1)
