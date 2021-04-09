rm(list = ls(all = TRUE))

library(MOEADps)
library(feather)
library(emoa)
# library(parallel)
require(compiler)
enableJIT(3)

#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# cores <-  2
# cl <- makeCluster(cores)

setwd("~/mazda_problem/")

source("~/MOEADr/R/utils.R")
source("~/MOEADr/R/moead.R")
source('~/MOEADr/exp_scripts/MAZDA.R')
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
if (!file.exists("output")){
  dir.create("output")
}

# Characteristics of the problem
n_variables = 222
dimension <- n_variables
n_objectives = 2
n_constraints = 54

# Parameters for execution
n_individuals = 300
n_iterations = 100
n_runs = 10

# Reading the possible discrete values
discrete = read.table("~/MultiObjectiveProjects/problems/mazda_mop/misc/DiscreteValues3.txt",col.names = paste0("V",seq_len(18)), sep = ",",fill = TRUE)

# Generating the minimum and maximum of each variable
maximum = c(0, nrow = n_variables)
minimum = c(0, nrow = n_variables)
for (i in 1:n_variables){
  maximum[i] = max(discrete[i,], na.rm = TRUE)
  minimum[i] = min(discrete[i,], na.rm = TRUE)
}

problem.1 <- list(name       = "problem.car",  # Function that executes the MOP
                  xmin       = minimum,    # minimum parameter value for each dimension
                  xmax       = maximum,     # maximum parameter value for each dimension
                  constraints = list(name = "my_constraints"),
                  m          = n_objectives)              # Number of objectives

## 1 - Decomposition
decomp <- list(name = "sld", H = 299, .nobj = 2)

## 2 - Neighbors
neighbors <- preset_moead("moead.de")$neighbors
neighbors$T <- 300 * 0.2
neighbors$delta.p <- 0.9813

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
variation <- list(list(name  = "diffmut",
                       basis  = "rand", phi = 0.5),
                  list(name  = "polymut",
                       etam  = 56, pm = 0.55 ),
                  list(name  = "truncate"))

## 8 - Show
showpars  <- list(show.iters = "dots",
                  showevery  = 30000)

## 9 - Constraint
constraint <- list(name = "unfeasible_exploration",
                   penalties = c(0, 1, 1000))

constraint <- list(name = "dynamic",
                   C = 0.05,
                   alpha = 2)



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
  dir.name <- paste0("~/gecco2021/mazda_ps_iter_", i, "/")
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
  
  
  
  dir.name <- paste0("~/gecco2021/mazda_moead_iter_", i, "/")
  if (!dir.exists(dir.name))
    dir.create(dir.name)
  moead.de <- moeadps_compiled(
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
    saving.dir = dir.name
  )
  

  
  
}