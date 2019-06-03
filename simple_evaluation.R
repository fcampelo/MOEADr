rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
# library(stringr)
# library(ecr)
# library(mco)
library(feather)
library(withr)
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)
# source("load.DTLZ.function.R")
# source("resource.allocation.R")
# source("utils.R")
# source("moead.R")

results <- data.frame()
for(i in 1:7){

moead.norm <- loadPlotData(name = "moead.norm", j = i)  
moead.norm.inverse <- loadPlotData(name = "moead.norm.inverse", j = i)  
moead.norm.tournament <- loadPlotData(name = "moead.norm.tournament", j = i)  
  
class(moead.norm) <- "moead"
class(moead.norm.inverse) <- "moead"
class(moead.norm.tournament) <- "moead"
# moead.de$Y.original <- moead.de$Y
# moead.rad$Y.original <- moead.rad$Y
# moead.dra$Y.original <- moead.dra$Y
# moead.random$Y.original <- moead.random$Y
moead.norm$Y.original <- moead.norm$Y
moead.norm.inverse$Y.original <- moead.norm.inverse$Y
moead.norm.tournament$Y.original <- moead.norm.tournament$Y

# ref <- rbind(moead.de$Y, moead.rad$Y, moead.dra$Y,moead.random$Y, moead.norm$Y)
Yref <-
  as.matrix(read.table(paste0("../inst/extdata/pf_data/DTLZ4.2D.pf")))
colnames(Yref) <- c("f1", "f2")

ref1 <- rbind(moead.norm.inverse$Y, moead.norm.tournament$Y, moead.norm$Y)
ref2 <- rbind(moead.norm.inverse$Y, moead.norm.tournament$Y, moead.norm$Y, Yref)

# moead.de$Y.scaled <- scaling_Y(moead.de$Y, ref)
# moead.rad$Y.scaled <- scaling_Y(moead.rad$Y, ref)
# moead.dra$Y.scaled <- scaling_Y(moead.dra$Y, ref)
# moead.random$Y.scaled <- scaling_Y(moead.random$Y, ref)
moead.norm$Y.scaled <- scaling_Y(moead.norm$Y, ref1)
moead.norm.inverse$Y.scaled <- scaling_Y(moead.norm.inverse$Y, ref1)
moead.norm.tournament$Y.scaled <- scaling_Y(moead.norm.tournament$Y, ref1)
Yref <- scaling_Y(Yref, ref2)
# moead.de$Y <- moead.de$Y.scaled
# moead.rad$Y <- moead.rad$Y.scaled
# moead.dra$Y <- moead.dra$Y.scaled
# moead.random$Y <- moead.random$Y.scaled
moead.norm$Y <- moead.norm$Y.scaled
moead.norm.inverse$Y <- moead.norm.inverse$Y.scaled
moead.norm.tournament$Y <- moead.norm.tournament$Y.scaled

# plot(moead.de)
# plot(moead.rad)
# plot(moead.dra)
# plot(moead.random)


# print(summary(moead.de, ref.point = c(1.1,1.1), ref.front = moead.dra$Y))
# print(summary(moead.rad, ref.point = c(1.1,1.1), ref.front = moead.dra$Y))
# print(summary(moead.dra, ref.point = c(1.1,1.1), ref.front = moead.dra$Y))
# print(summary(moead.random, ref.point = c(1.1,1.1), ref.front = moead.dra$Y))

norm <- data.frame(summary(moead.norm, ref.point = c(1.1,1.1), ref.front = Yref), name = "norm.data")
norm.inverse <- data.frame(summary(moead.norm.inverse, ref.point = c(1.1,1.1), ref.front = Yref), name = "norm.inverse.data")
norm.tournament <- data.frame(summary(moead.norm.tournament, ref.point = c(1.1,1.1), ref.front = Yref), name = "norm.tournament")
results <- rbind(results, norm, norm.inverse, norm.tournament)
                     

# moead.de$Y <- moead.de$Y.original
# moead.rad$Y <- moead.rad$Y.original
# moead.dra$Y <- moead.dra$Y.original
# moead.random$Y <- moead.random$Y.original
# moead.norm$Y <- moead.norm$Y.original
}

print(aggregate(results$hv, median, by = list(results$name)))
print(aggregate(results$igd, median, by = list(results$name)))

agg.hv <- aggregate(results$hv, median, by = list(results$name))
agg.igd <- aggregate(results$igd, median, by = list(results$name))

id.norm <- which(results[results$name == "norm.data",]$hv == agg.hv[agg.hv$Group.1=="norm.data",]$x)
id.norm.inverse <- which(results[results$name == "norm.inverse.data",]$hv == agg.hv[agg.hv$Group.1=="norm.inverse.data",]$x)
id.norm.tournament <- which(results[results$name == "norm.tournament",]$hv == agg.hv[agg.hv$Group.1=="norm.tournament",]$x)


####
moead.norm <- loadPlotData(name = "moead.norm", j = id.norm)
moead.norm.inverse <- loadPlotData(name = "moead.norm.inverse", j = id.norm.inverse)
moead.norm.tournament <- loadPlotData(name = "moead.norm.tournament", j = id.norm.tournament)
# plot(moead.norm$Y)
# plot(moead.norm.inverse$Y)
# plot(moead.norm.tournament$Y)
visuEvol(moead.norm, "moead.norm")
visuEvol(moead.norm.inverse, "moead.norm.inverse")
visuEvol(moead.norm.tournament, "moead.norm.tournament")
