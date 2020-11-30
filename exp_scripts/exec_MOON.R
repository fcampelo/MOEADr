rm(list = ls(all = TRUE))

library(MOEADps)
library(emoa)
library(ggplot2)
library(parallel)
require(compiler)
enableJIT(3)
moeadps_compiled <- cmpfun(moeadps)
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))

cores <-  4
cl <- makeCluster(cores)

setwd("~/moon_problem/")

source("~/MOEADr/R/utils.R")
source('~/MOEADr/exp_scripts/MOON.R')

constraint_unfeasible_exploration <- function(penalties, bigZ, bigV, ...)
{
  # ========== Error catching and default value definitions
  assertthat::assert_that(
    identical(dim(bigZ), dim(bigV)))
  # ==========
  
  e = parent.frame(2)
  total_gen = (e$stopcrit[[1]]$maxeval)/325
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
n_variables = 2
n_objectives = 3
n_constraints = 2

# Parameters for execution
n_individuals = 25 #25 -> 325
n_iterations = 100
n_runs = 10

# Creating Variable Bounds
maximum = c(1, 1)
minimum = c(0, 0)

problem.1 <- list(name       = "problem.moon",  # Function that executes the MOP
                  xmin       = minimum,    # minimum parameter value for each dimension
                  xmax       = maximum,     # maximum parameter value for each dimension
                  constraints = list(name = "my_constraints"),
                  m          = n_objectives)              # Number of objectives

## 1 - Decomposition
loaded.weights.3objs <-
  data.matrix(
    read.csv(
      "~/MOEADr/SOBOL-3objs-500wei.ws",
      header = F,
      stringsAsFactors = FALSE,
      sep = " "
    )
  )
decomp.loaded.3 <- list(name = "loaded", W = loaded.weights.3objs)

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
stopcrit  <- list(list(name  = "maxeval",
                       maxeval  = 30000))

## 7 - Variation Operators
variation = preset_moead("moead.de")$variation
variation[[2]]$pm = 1 / 2
neighbors <- preset_moead("moead.de")$neighbors
neighbors$T <- 100

## 8 - Show
showpars  <- list(show.iters = "none",
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
  
 moead.random <- moeadps_compiled(problem  = problem.1,
                        decomp = decomp.loaded.3,
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
  
  moead.de <- moeadps_compiled(problem  = problem.1,
                        decomp = decomp.loaded.3,
                        neighbors = neighbors,
                        aggfun = aggfun,
                        scaling = scaling,
                        constraint = constraint,
                        stopcrit = stopcrit,
                        update = update,
                        variation = variation,
                        showpars = showpars,
                        seed     = floor(runif(1)*1000))
                        
  # Calculate the hypervolume only with feasible points
  # No feasible solutions
  if(max(moead.random$V$v == 0) == 0){
    hyper[i] = 0
  }
  # At least one feasible solution
  else{
    hyper[i] = dominated_hypervolume(t(moead.random$Archive$Y[which(moead.random$V$v == 0),]), (c(1,0,1)))
    cat("Iteration: ", i,"Hyper: ", hyper[i])
  }
  
  if(max(moead.de$V$v == 0) == 0){
    hyper[i] = 0
  }
  # At least one feasible solution
  else{
    hyper[i] = dominated_hypervolume(t(moead.de$Archive$Y[which(moead.de$V$v == 0),]), (c(1,0,1)))
    cat("Iteration: ", i,"Hyper: ", hyper[i])
  }
}

stopCluster(cl)

# gerate_graph <- function(pos) {
#   aux <- as.data.frame(pos)
#   
#   p <-
#     plot_ly(
#       aux,
#       x = aux$f1,
#       y = aux$f2,
#       z = aux$f3,
#       color = aux$f1,
#       colors =  c('#4AC6B7', '#1972A4'),
#       marker = list(size = 4, sizeref = 2)
#     ) %>%
#     add_markers() %>%
#     layout(scene = list(
#       xaxis = list(title = 'X'),
#       yaxis = list(title = 'Y'),
#       zaxis = list(title = 'Z')
#     ))
#   p
# }
# library(plotly)
# gerate_graph(moead.random$Archive$Y)
# gerate_graph(moead.de$Archive$Y)
