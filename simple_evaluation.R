rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
library(feather)
library(withr)
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)
fun.names1 <- list()

number.fun <- 1
repetitions <- 1

for (i in 1:number.fun) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}

results <- data.frame()
#ref1 <- data.frame()
for (fun in fun.names1) {
  ref1 <- data.frame()
  for (i in 1:repetitions) {
    moead.de <- loadPlotData(name = paste0(fun, "moead.de"), j = i)
    moead.norm <- loadPlotData(name = paste0(fun, "moead.norm"), j = i)
    moead.norm.inverse <-
      loadPlotData(name = paste0(fun, "moead.norm.inverse"), j = i)
    moead.norm.tournament <-
      loadPlotData(name = paste0(fun, "moead.norm.tournament"),
                   j = i)
    
    ref1 <-
      rbind(ref1, moead.norm.inverse$Y,
            moead.norm.tournament$Y,
            moead.de$Y,
            moead.norm$Y)
  }
  
  for (i in 1:repetitions) {
    moead.de <- loadPlotData(name = paste0(fun, "moead.de"), j = i)
    moead.norm <- loadPlotData(name = paste0(fun, "moead.norm"), j = i)
    moead.norm.inverse <-
      loadPlotData(name = paste0(fun, "moead.norm.inverse"), j = i)
    moead.norm.tournament <-
      loadPlotData(name = paste0(fun, "moead.norm.tournament"),
                   j = i)
    
    class(moead.de) <- "moead"
    class(moead.norm) <- "moead"
    class(moead.norm.inverse) <- "moead"
    class(moead.norm.tournament) <- "moead"
    
    Yref <-
      as.matrix(read.table(paste0(
        "../inst/extdata/pf_data/", fun, ".2D.pf"
      )))
    colnames(Yref) <- c("f1", "f2")
  
    de <-
      data.frame(summary(
        moead.de,
        scaling.reference = ref1,
        ref.point = c(1.1, 1.1),
        ref.front = Yref,
      ), name = "de.data")
    norm <-
      data.frame(summary(
        moead.norm,
        scaling.reference = ref1,
        ref.point = c(1.1, 1.1),
        ref.front = Yref,
      ), name = "norm.data")
    norm.inverse <-
      data.frame(summary(
        moead.norm.inverse,
        scaling.reference = ref1,
        ref.point = c(1.1, 1.1),
        ref.front = Yref
      ),
      name = "norm.inverse.data")
    norm.tournament <-
      data.frame(summary(
        moead.norm.tournament,
        scaling.reference = ref1,
        ref.point = c(1.1, 1.1),
        ref.front = Yref
      ),
      name = "norm.tournament")  
  }
  results <- rbind(results, cbind(rbind(de, norm, norm.inverse, norm.tournament), fun))
#print(results)
#exit()
}
print(aggregate(results$hv, median, by = list(results$name, results$fun)))
print(aggregate(results$igd, median, by = list(results$name, results$fun)))

agg.hv <- aggregate(results$hv, median, by = list(results$name, results$fun))
agg.igd <- aggregate(results$igd, median, by = list(results$name, results$fun))

id.de <-
  which(results[results$name == "de.data", ]$hv == agg.hv[agg.hv$Group.1 ==
                                                              "de.data", ]$x)
id.norm <-
  which(results[results$name == "norm.data", ]$hv == agg.hv[agg.hv$Group.1 ==
                                                              "norm.data", ]$x)
id.norm.inverse <-
  which(results[results$name == "norm.inverse.data", ]$hv == agg.hv[agg.hv$Group.1 ==
                                                                      "norm.inverse.data", ]$x)
id.norm.tournament <-
  which(results[results$name == "norm.tournament", ]$hv == agg.hv[agg.hv$Group.1 ==
                                                                    "norm.tournament", ]$x)


####
moead.de <- loadPlotData(name = paste0(fun, "moead.de"), j = id.de)
moead.norm <- loadPlotData(name = paste0(fun, "moead.norm"), j = id.norm)
moead.norm.inverse <-
  loadPlotData(name = paste0(fun, "moead.norm.inverse"), j = id.norm.inverse)
moead.norm.tournament <-
  loadPlotData(name = paste0(fun, "moead.norm.tournament"),
               j = id.norm.tournament)

moead.de$Y <- scaling_Y(moead.de$Y, ref1)
moead.norm$Y <- scaling_Y(moead.norm$Y, ref1)
moead.norm.inverse$Y <- scaling_Y(moead.norm.inverse$Y, ref1)
moead.norm.tournament$Y <- scaling_Y(moead.norm.tournament$Y, ref1)
plot(moead.de$Y, main = "moead.de")
plot(moead.norm$Y, main = "moead.norm")
plot(moead.norm.inverse$Y, main = "moead.norm.inverse")
plot(moead.norm.tournament$Y, main = "moead.tournament")
# visuEvol(moead.norm, paste0(fun, "moead.de"))
# visuEvol(moead.norm,paste0(fun, "moead.de"))
# visuEvol(moead.norm.inverse, paste0(fun, "moead.de"))
# visuEvol(moead.norm.tournament, "moead.norm.tournament")
