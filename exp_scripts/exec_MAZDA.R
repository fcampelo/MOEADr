library(MOEADps)
library(emoa)
library(ggplot2)

source('~/MOEADr/exp_scripts/MAZDA.R')
source('~/MOEADr/R/updt_best.R')

constraint_unfeasible_exploration <- function(penalties, bigZ, bigV, ...)
{
  # ========== Error catching and default value definitions
  assertthat::assert_that(
    identical(dim(bigZ), dim(bigV)))
  # ==========

  e = parent.frame(2)
  total_gen = e$stopcrit[[1]]$maxiter
  current_gen = e$iter

  # Determine the parameter based on the current generation
  if(current_gen < floor(total_gen/4)){
    K <- penalties[1]
  }
  else if(current_gen < floor(total_gen/2)){
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


# Creating the output directory if necessary
if (!file.exists("output")){
  dir.create("output")
}

# Characteristics of the problem
n_variables = 222
n_objectives = 2
n_constraints = 54

# Parameters for execution
n_individuals = 300
n_iterations = 100
n_runs = 1

# Reading the possible discrete values
discrete = read.table(paste(getwd(), "~//DiscreteValues3.txt", sep="/"),col.names = paste0("V",seq_len(18)), sep = ",",fill = TRUE)

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
decomp <- list(name = "SLD",H = n_individuals - 1)

## 2 - Neighbors
neighbors <- preset_moead("moead.de")$neighbors
neighbors$T <- 60

## 3 - Aggregation function
aggfun <- list(name = "wt")

## 4 - Update strategy
update <- preset_moead("moead.de")$update
update$UseArchive = TRUE

## 5 - Scaling
scaling <- list(name = "simple")

## 6 - Stop criterion
stopcrit  <- list(list(name  = "maxiter",
                       maxiter  = n_iterations))

## 7 - Variation Operators
variation = preset_moead("moead.de")$variation
variation[[2]]$pm = 1 / dimension

## 8 - Show
showpars  <- list(show.iters = "dots",
                  showevery  = 5)

## 9 - Constraint
constraint <- list(name = "unfeasible_exploration",
                  penalties = c(0,1,1000))

## 11 - Resource Allocation
resource.allocation.RANDOM <-
  list(
    name = "random",
    dt = 0,
    selection = "n",
    n = 48
  )

## 10 - Execution
hyper = rep(0,n_runs)
hyperteste = rep(0,n_runs)
besthyper = -1
for (i in 1:n_runs){
  file.create(sprintf("output/MyArchive%d.txt",i))
  results <- moeadps(problem  = problem.1,
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
                   seed     = floor(runif(1)*1000))

  # Scaling the objectives
  results2 = results
  results2$Y[,1] = results2$Y[,1]/74
  results2$Y[,2] = results2$Y[,2] - 2

  # Calculate the hypervolume only with feasible points
  # No feasible solutions
  if(max(results$V$v == 0) == 0){
    hyper[i] = 0
  }
  # At least one feasible solution
  else{
    hyper[i] = dominated_hypervolume(t(results2$Y[which(results$V$v == 0),]), (c(0,1.1)))
    cat("Iteration: ", i,"Hyper: ", hyper[i])
  }
}
