rm(list = ls(all = TRUE))

library(MOEADps)
library(feather)
library(emoa)
require(compiler)
enableJIT(3)

setwd("~/moon_problem/")

source("~/MOEADr/R/utils.R")
source('~/MOEADr/exp_scripts/MOON.R')

moeadps_compiled <- cmpfun(moeadps)
constraint_unfeasible_exploration <-
  function(penalties, bigZ, bigV, ...)
  {
    # ========== Error catching and default value definitions
    assertthat::assert_that(identical(dim(bigZ), dim(bigV)))
    # ==========
    
    e = parent.frame(2)
    total_nfe = (e$stopcrit[[1]]$maxeval)
    current_nfe = e$nfe
    
    # Determine the parameter based on the current generation
    if (current_nfe < floor(total_nfe / 4)) {
      K <- penalties[1]
    }
    else if (current_nfe < floor(total_nfe / 2)) {
      K <- penalties[2]
    }
    else{
      K <- penalties[3]
    }
    # Calculate penalized values
    bigZV <- bigZ + K * bigV
    
    # Get the selection matrix for all neighborhoods
    sel.indx <- t(apply(bigZV,
                        MARGIN = 2,
                        FUN    = order))
    
    return(sel.indx)
  }

constraint_dynamic <- function(C, alpha, bigZ, bigV, ...)
{
  # ========== Error catching and default value definitions
  assertthat::assert_that(
    identical(dim(bigZ), dim(bigV)))
  # ==========
  
  # Calculate dynamic parameter
  K <- (C*parent.frame(2)$iter)^alpha
  
  # Calculate penalized values
  bigZV <- bigZ + K * bigV
  
  # Get the selection matrix for all neighborhoods
  sel.indx <- t(apply(bigZV,
                      MARGIN = 2,
                      FUN    = order))
  
  return(sel.indx)
}


# Creating the output directory if necessary
if (!file.exists("output")) {
  dir.create("output")
}

# Characteristics of the problem
n_variables = 2
n_objectives = 3
n_constraints = 2

# Parameters for execution
# n_individuals = 25 #25 -> 325
# n_iterations = 100
n_runs = 10

# Creating Variable Bounds
maximum = c(1, 1)
minimum = c(0, 0)

problem.1 <-
  list(
    name       = "problem.moon",
    # Function that executes the MOP
    xmin       = minimum,
    # minimum parameter value for each dimension
    xmax       = maximum,
    # maximum parameter value for each dimension
    constraints = list(name = "my_constraints"),
    m          = n_objectives
  )              # Number of objectives

## 1 - Decomposition
decomp <- list(name = "sld", H = 23, .nobj = 3)

## 2 - Neighbors
neighbors <- preset_moead("moead.de")$neighbors
neighbors$T <- 300 * 0.2
neighbors$delta.p <- 0.471

## 3 - Aggregation function
aggfun <- list(name = "wt")

## 4 - Update strategy
update <- preset_moead("moead.de")$update
update$UseArchive = TRUE

## 5 - Scaling
scaling <- list(name = "simple")

## 6 - Stop criterion
stopcrit  <- list(list(name  = "maxeval",
                       maxeval  = 100000))

## 7 - Variation Operators
# variation <- list(
#   list(name  = "sbx",
#        etax  = 74, pc = 0.65),
#   list(name  = "polymut",
#        etam  = 26, pm = 0.15),
#   list(name  = "truncate")
#   
# )
variation <- preset_moead("moead.de")$variation
variation[[2]]$pm = 1/2#dimension

## 8 - Show
showpars  <- list(show.iters = "dots",
                  showevery  = 60000)

## 9 - Constraint
# constraint <- list(name = "unfeasible_exploration",
#                    penalties = c(1, 3, 1000))
constraint <- list(name = "dynamic",
                   C = 0.05,
                   alpha = 2)

# constraint <- list(name = "penalty", beta = 1)

## 11 - Resource Allocation
resource.allocation.RANDOM <-
  list(
    name = "random",
    dt = 0,
    selection = "n",
    n = 27,
    period = -1
  )



## 10 - Execution

for (i in 1:n_runs) {
  print("iter")
  dir.name <- paste0("~/gecco2021/moon_ps_iter_", i, "/")
  if (!dir.exists(dir.name))
    dir.create(dir.name)

  moead.random <- moeadps_compiled(
    problem  = problem.1,
    decomp = decomp,
    neighbors = neighbors,
    aggfun = aggfun,
    scaling = scaling,
    constraint = constraint,
    stopcrit = stopcrit,
    update = update,
    variation = variation,
    showpars = showpars,
    resource.allocation = resource.allocation.RANDOM,
    saving.dir = dir.name
  )
  
  

  
}

# stopCluster(cl)
