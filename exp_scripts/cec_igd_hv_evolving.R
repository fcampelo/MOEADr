rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
library(feather)
library(withr)
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)
fun.names1 <- list()
source("visualization_tools.R")
repetitions <- 21

results_fixed <- read_feather("~/myMOEADr/results_fixed")

names <- c(
  "moead.random_fixed.1.0",
  "moead.random_fixed.0.8",
  "moead.random_fixed.0.6",
  "moead.random_fixed.0.4",
  "moead.random_fixed.0.2",
  "moead.random_fixed.0.1"
)

strategy <-
  c("100%",
    "80%",
    "60%",
    "40%",
    "20%",
    "10%")

ref.point <- c(1,1)

fun.names1 <- list()
for (i in 1:7) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}
for (i in 1:7) {
  fun.names1[[length(fun.names1) + 1]]  = paste0("UF", i)
}

for (fun in fun.names1) {
  temp <- results_fixed[results_fixed$fun == fun, ]
  i <- 1
  for (name in names) {
    temp2 <- temp[temp$name == name,]
    moea <-
      loadPlotData(name = paste0(fun, name),
                   j = which(temp2$igd == median(temp2$igd)))
    
    n.iter <- as.numeric(moea$n.iter)
    my_hv <- data.frame()
    n.iter <- n.iter
    for (i in 0:n.iter) {
      idxs <- ((350 * i) + 1):(350 * (i + 1))
      PF <-
        cbind(
          moea$plot.paretofront$f1[idxs],
          moea$plot.paretofront$f2[idxs]
        )
      #
      my_hv <- emoa::dominated_hypervolume(t(pf), ref=ref.point)
    }
    png(file = paste0("~/hv_evolving/",fun, name, "_hv.png"))
    plot(
      x = nrow(my_hv):1,
      y = my_hv$X,
      xlab = "Iteration",
      ylab = "HV",
      main = paste(fun, strategy[i])
    )
    dev.off()
    i <- i + 1
    
    exit()
    
  }
}