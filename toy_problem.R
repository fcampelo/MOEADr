# rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
library(stringr)
library(ecr)
library(mco)
library(feather)
library(withr)
# lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)
source("load.DTLZ.function.R")
source("resource.allocation.R")
source("utils.R")
source("moead.R")
source("summary_moead.R")

algorithms <- c("moead.de")

#uniform weight
resource.allocation.NORM <- list(name = "norm", dt = 20, selection = "random")

f1 <- function(X) {
  (1 / (sum(X ^ 2) + 1)) - 1.1 * exp(-sum(X ^ 2))
}


f2 <- function(X) {
  b <- c(3, 1)
  return (-1*(sum(-1 * ((X + b) ^ 2))))
}

# f3 <- function(X){
#   return(0.5*sum((X^2))+sum(sin(X^2)))
# }
# 
# f4 <- function(X){
#   return (0.8 + sum(0.3*X))
# }

problem.sr <- function(X) {
  t(apply(X, MARGIN = 1,
          FUN = function(X) { c(f1(X), f2(X)) } 
  ))
}

problem.1 <- list(name       = "problem.sr",  # function that executes the MOP
                  xmin       = c(-30,-3),    # minimum parameter value for each dimension
                  xmax       = c(30,3),     # maximum parameter value for each dimension
                  m          = 2)     


library(MOEADr)
scaling <- list()
scaling$name <- "simple"
stopcrit  <- list(list(name    = "maxeval",
                       maxeval = 30000))

for (i in 1:1){
original <- moead(problem  = problem.1,
              preset   = preset_moead("moead.de"),
              showpars = list(show.iters = "none"), 
              stopcrit = stopcrit,
              scaling = scaling,
              seed     = i)


normRA <- moead(problem  = problem.1,
                 preset   = preset_moead("moead.de"),
                 showpars = list(show.iters = "none"), 
                 stopcrit = stopcrit,
                 scaling = scaling,
                 seed     = i,
                 resource.allocation = resource.allocation.NORM)

plot(original, suppress.pause = T)
plot(normRA, suppress.pause = T)



Y.temp <- rbind(original$Y, normRA$Y)

original$Y.original <- original$Y
normRA$Y.original <- normRA$Y

Y.min <-apply(Y.temp, 2, min)
Y.max <-apply(Y.temp, 2, max)

original$Y.scaled <- (original$Y - Y.min)/(Y.max - Y.min)
normRA$Y.scaled <- (normRA$Y - Y.min)/(Y.max - Y.min)
original$Y <- original$Y.scaled
normRA$Y <- normRA$Y.scaled

print(summary(original, ref.point = c(1.002857,1.002857), ref.front = normRA$Y))
print(summary(normRA, ref.point = c(1.002857,1.002857), ref.front = normRA$Y))
}

# original$Y <- original$Y.original
# normRA$Y <- normRA$Y.original
