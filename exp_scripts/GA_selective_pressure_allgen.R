rm(list = ls(all = TRUE))

library("Kendall")
library(smoof)
library(ggplot2)

scaling <- function(Y){
  MinP <- min(Y)
  MaxP <- max(Y)
  Y    <- (Y - MinP) / (MaxP - MinP + 1e-16)
  return (Y)
}

dim <- 5

f <- smoof::makeRastriginFunction(dim)
f <- smoof::makeSphereFunction(dim)
# f <- smoof::makeAdjimanFunction()
# f <- smoof::makeBentCigarFunction(100)

par.set = ParamHelpers::getParamSet(f)
lower <- as.numeric(getLower(par.set))
upper <- as.numeric(getUpper(par.set))

# hyperparameters
npop <- 100 # population size
iterations <- 99 + 1
prob.size <- dim
tour.size <- 2
wind.size <- 5
n <- 2
nr <- 10
# elitism <- FALSE
elitism <- TRUE
# select <- "random"
# select <- "best"
# select <- "random.2"
select <- "roullete"
# select <- "roullete.scaled"
# select <- "tournament"
# select <- "worse"
f.eval <- npop * iterations


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
    (fitnessess - min(fitnessess)) / (max(fitnessess) - min(fitnessess) + 1e-50)
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
# offspring.count <- rep(0, npop)
offspring.count <- matrix(0, nrow = npop, ncol = wind.size)
# fitness.cumulative <- rep(0, npop)
fitness.cumulative <- matrix(0, nrow = npop, ncol = wind.size)

pop <-
  matrix(runif(
    n = 1:(npop * prob.size),
    min = lower,
    max = upper
  ), ncol = prob.size)
fitnessess <- apply(pop, 1, f)
# main loop

eval <- npop

distance.pop[1] <- mean(abs(sum(dist(pop))))

iter <- 0
while (eval < f.eval) {
  offspring.count[,(iter%%wind.size)+1] <- 0
  iter <- iter + 1
  # elitism
  best.id <- selection.best(fitnessess, 1)
  best.solution <- pop[best.id,]
  best.fitness <- min(fitnessess)
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
  else if (select == "random.2") {
    idx.tour <- sample(1:npop, replace = T)[1]
    idx.tour <- c(idx.tour, selection.best(fitnessess, 1))
  }
  else {
    idx.tour <- selection.best(fitnessess, n)
  }
  
  new.pop <- pop
  
  # crossover
  all.idx <- sample(npop)
  temp <- idx.tour
  for(i in 1:(length(idx.tour)/2)){
    
    idxs <- sample(x = temp, size = 2)
    out <- cx.uniform(new.pop[idxs[1],], new.pop[idxs[2],])
    new.pop[idxs[1],] <- out$candidate1
    new.pop[idxs[2],] <- out$candidate2
    temp <- temp[-idxs]
  }
  
  # apply mutation
  temp <- idx.tour
  for (i in 1:length(idx.tour)){
    idx <- sample(x = temp, size = 1)
    new.pop[idx,] <- mut.gaussian(new.pop[idx,])
    temp <- temp[-idx]
  }
  

  # measure quality
  new.fitnessess <- apply(new.pop, 1, f)
  
  fitness.cumulative[,(iter%%wind.size)+1] <- fitnessess
  for(i in sample(1:npop)){ # new
      if(new.fitnessess[i] < fitnessess[i]){
        pop[i,] <- new.pop[i,]
        fitnessess[i] <- new.fitnessess[i]
        offspring.count[i,(iter%%wind.size)+1] <- offspring.count[i,(iter%%wind.size)+1] + 1
        fitness.cumulative[i,(iter%%wind.size)+1] <- fitnessess[i]
      }
    }

  
  # #elitism
  # if(isTRUE(elitism)){
  #   temp.id <- selection.worse(fitnessess, 1)
  #   pop[temp.id,] <- best.solution
  #   fitnessess[temp.id] <- best.fitness
  #   if (offspring.count[temp.id] < 0) offspring.count[temp.id] <- offspring.count[temp.id] - 1
  #   offspring.count[best.id] <- offspring.count[best.id] + 1
  # }

  eval <- eval + npop
  
  distance.pop[iter] <- mean(abs(sum(dist(pop))))  
  
  if (var(rowSums(apply(X = fitness.cumulative, MARGIN = 2, scaling))) != 0){
    if (sum(rowSums(offspring.count)) != 0){
    pressure.offspring[iter - 1] <-
      Kendall(rowSums(offspring.count), rowSums(apply(X = fitness.cumulative, MARGIN = 2, scaling)))$tau[1]
    }
  }
  
  # best.fitness <- min(fitnessess)
  #   if(round(best.fitness,1) == 0){
  #   break
  # }
  # else if(var(fitnessess) == 0){
  #   break
  # }
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
  cbind(metric = (distance.pop[1:iter] - min(distance.pop[1:iter])) / (max(distance.pop[1:iter]) - min(distance.pop[1:iter])), name = "Diversity all vs all")

df.pressure <-
  cbind(metric = pressure.offspring[1:iter], name = "Pressure")

plot.data <-
  data.frame(
    iterations = 1:iter,rbind(df.pressure, df.distance) 
  )
plot.data$metric=as.numeric(levels(plot.data$metric))[plot.data$metric]


if (select == "tournament"){
  

print(
  ggplot(plot.data, aes(iterations, metric)) +
    ylim(-1, 1) + 
    geom_point(aes(color = name, shape = name)) +
    ggtitle(paste("Selective pressure in GA using selection - ", select, tour.size, " - in the Sphere problem. Output: ", round(best.fitness, 3)))
)
  filename = paste0("~/Desktop/",select,"_",tour.size,".png")
}
if (select != "tournament"){
  print(
    ggplot(plot.data, aes(iterations, metric)) +
      ylim(-1, 1) + 
      geom_point(aes(color = name, shape = name)) +
      ggtitle(paste("Selective pressure in GA using selection - ", select, " - in the Sphere problem. Output: ", round(best.fitness, 3)))
  )
  filename = paste0("~/Desktop/",select,".png")
}

ggsave(filename = filename,
       dpi = 600,
       width = 9)
