rm(list = ls(all = TRUE))

library("Kendall")
library(smoof)
library(ggplot2)


dim <- 5

# f <- smoof::makeRastriginFunction(100)
f <- smoof::makeSphereFunction(dim)
# f <- smoof::makeAdjimanFunction()
# f <- smoof::makeBentCigarFunction(100)

par.set = ParamHelpers::getParamSet(f)
lower <- as.numeric(getLower(par.set))
upper <- as.numeric(getUpper(par.set))

# hyperparameters
npop <- 100 # population size
iterations <- 200
prob.size <- dim
tour.size <- 10
n <- 2
# select <- "random"
# select <- "best"
select <- "roullete"
# select <- "roullete.scaled"
# select <- "tournament"
# select <- "worse"
f.eval <- npop * iterations
size.window <- 5

set.seed(42)
#### GA

## operators
# crossover operator
cx.uniform <- function(candidate1, candidate2, indpb = 0.1) {
  mask <- runif(length(candidate1)) < indpb
  temp <- candidate1
  candidate1[mask] <- candidate2[mask]
  candidate2[mask] <- temp[mask]
  return(list(candidate1 = candidate1, candidate2 = candidate2))
}

# mutation operator
mut.gaussian <- function(candidate, indpb = 0.1) {
  mask <- as.numeric(runif(length(candidate)) < indpb)
  candidate + mask * rnorm(length(candidate))
}

# selection operator
selection.tour <- function(npop, tour.size, fitnessess) {
  selected <- rep(NA, npop)
  for (i in 1:npop) {
    idxs <- sample(1:npop, tour.size)
    selected[i] <- idxs[which.min(fitnessess[idxs])]
  }
  selected
}

selection.roullete.scaled <- function(npop, fitnessess) {
  selected <- rep(NA, npop)
  value <-
    (fitnessess - min(fitnessess)) / (max(fitnessess) - min(fitnessess))
  for (i in 1:npop) {
    idxs <- sample(1:npop,
                   size = npop,
                   prob = value,
                   replace = T)
    selected[i] <- idxs[which.min(fitnessess[idxs])]
  }
  selected
}

selection.roullete <- function(npop, fitnessess) {
  selected <- rep(NA, npop)
  for (i in 1:npop) {
    idxs <- sample(1:npop,
                   size = npop,
                   prob = fitnessess,
                   replace = T)
    selected[i] <- idxs[which.min(fitnessess[idxs])]
  }
  selected
}

selection.best <- function(fitnessess, k) {
  match(sort(fitnessess, F), fitnessess)[1:k]
}

selection.worse <- function(fitnessess, k) {
  match(sort(fitnessess, T), fitnessess)[1:k]
}

# pressure metric data
pressure.offspring <- rep(NA, iterations)
distance.pop <- rep(NA, iterations)
offspring.count <- rep(0, npop)
fitness.cumulative <- rep(0, npop)

pop <-
  matrix(runif(
    n = 1:(npop * prob.size),
    min = lower,
    max = upper
  ), ncol = prob.size)
fitnessess <- apply(pop, 1, f)
# main loop
iter <- 1
eval <- npop

distance.pop[1] <- abs(sum(dist(pop)))

while (eval < f.eval) {
  
  # select solutions for change
  if (select == "tournament") {
    idx.tour <- selection.tour(npop, tour.size, fitnessess)
  }
  else if (select == "random") {
    idx.tour <- sample(1:npop, replace = T)
  }
  else if (select == "roullete") {
    idx.tour <- selection.roullete(npop, fitnessess)
  }
  else if (select == "roullete.scaled") {
    idx.tour <- selection.roullete.scaled(npop, fitnessess)
  }
  else if (select == "worse") {
    idx.tour <- selection.worse(fitnessess, n)
  }
  else {
    idx.tour <- selection.best(fitnessess, n)
  }
  
  new.pop <- pop
  
  # pressure metric data
  for (t in idx.tour) {
    offspring.count[t] <- offspring.count[t] + 1
  }
  
  all.idx <- sample(1:npop)
  while (length(all.idx) > 1) {
    idxs <- sample(all.idx, 2, replace = F)
    out <- cx.uniform(new.pop[idxs[1],], new.pop[idxs[2],])
    new.pop[idxs[1],] <- out$candidate1
    new.pop[idxs[2],] <- out$candidate2
    all.idx <- all.idx[-which(all.idx %in% idxs)]
  }
  
  # apply mutation
  new.pop <- t(apply(new.pop, 1, mut.gaussian))
  
  # measure quality
  new.fitnessess <- apply(new.pop, 1, f)
  
  pop[-idx.tour, ] <- new.pop[-idx.tour,]
  fitnessess[-idx.tour] <- new.fitnessess[-idx.tour]
  
  eval <- eval + npop
  iter <- iter + 1
  
  fitness.cumulative <- fitness.cumulative + (fitnessess - min(fitnessess)) / (max(fitnessess) - min(fitnessess))
  
  pressure.offspring[iter - 1] <-
    Kendall(offspring.count, fitness.cumulative)$tau[1]
  
  distance.pop[iter] <- abs(sum(dist(pop)))
}

# get best solution
best.solution <- pop[which.min(fitnessess),]
best.fitness <- min(fitnessess)

cat("iter: ",
    iter,
    "fitness value",
    best.fitness,
    "\n")

pressure.offspring[iter] <- pressure.offspring[iter-1]

df.distance <-
  cbind(metric = (distance.pop - min(distance.pop)) / (max(distance.pop) - min(distance.pop)), name = "Population distance")

df.pressure <-
  cbind(metric = pressure.offspring, name = "Pressure")

plot.data <-
  data.frame(
    iterations = 1:length(pressure.offspring),rbind(df.pressure, df.distance)
  )
plot.data$metric=as.numeric(levels(plot.data$metric))[plot.data$metric]

library(ggplot2)

print(
  ggplot(plot.data, aes(iterations, metric)) +
    ylim(-1, 1) + 
    geom_point(aes(color = name, shape = name)) 
)


