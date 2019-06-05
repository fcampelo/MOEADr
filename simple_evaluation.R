rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
library(feather)
library(withr)
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)
fun.names1 <- list()
for (i in 4:4) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}

results <- data.frame()
for (fun in fun.names1) {
  for (i in 1:1) {
    moead.norm <- loadPlotData(name = paste0(fun, "moead.norm"), j = i)
    moead.norm.inverse <-
      loadPlotData(name = paste0(fun, "moead.norm.inverse"), j = i)
    moead.norm.tournament <-
      loadPlotData(name = paste0(fun, "moead.norm.tournament"),
                   j = i)
    
    class(moead.norm) <- "moead"
    class(moead.norm.inverse) <- "moead"
    class(moead.norm.tournament) <- "moead"
    moead.norm$Y.original <- moead.norm$Y
    moead.norm.inverse$Y.original <- moead.norm.inverse$Y
    moead.norm.tournament$Y.original <- moead.norm.tournament$Y
    
    Yref <-
      as.matrix(read.table(paste0(
        "../inst/extdata/pf_data/", fun, ".2D.pf"
      )))
    colnames(Yref) <- c("f1", "f2")
    
    ref1 <-
      rbind(moead.norm.inverse$Y,
            moead.norm.tournament$Y,
            moead.norm$Y)
    ref2 <-
      rbind(moead.norm.inverse$Y,
            moead.norm.tournament$Y,
            moead.norm$Y,
            Yref)
    
    moead.norm$Y.scaled <- scaling_Y(moead.norm$Y, ref1)
    moead.norm.inverse$Y.scaled <- scaling_Y(moead.norm.inverse$Y, ref1)
    moead.norm.tournament$Y.scaled <-
      scaling_Y(moead.norm.tournament$Y, ref1)
    Yref <- scaling_Y(Yref, ref2)
    
    moead.norm$Y <- moead.norm$Y.scaled
    moead.norm.inverse$Y <- moead.norm.inverse$Y.scaled
    moead.norm.tournament$Y <- moead.norm.tournament$Y.scaled
    
    
    norm <-
      data.frame(summary(
        moead.norm,
        ref.point = c(1.1, 1.1),
        ref.front = Yref
      ), name = "norm.data")
    norm.inverse <-
      data.frame(summary(
        moead.norm.inverse,
        ref.point = c(1.1, 1.1),
        ref.front = Yref
      ),
      name = "norm.inverse.data")
    norm.tournament <-
      data.frame(summary(
        moead.norm.tournament,
        ref.point = c(1.1, 1.1),
        ref.front = Yref
      ),
      name = "norm.tournament")
    results <- rbind(results, norm, norm.inverse, norm.tournament)
    
  }
}
print(aggregate(results$hv, median, by = list(results$name)))
print(aggregate(results$igd, median, by = list(results$name)))

agg.hv <- aggregate(results$hv, median, by = list(results$name))
agg.igd <- aggregate(results$igd, median, by = list(results$name))

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
moead.norm <- loadPlotData(name = "moead.norm", j = id.norm)
moead.norm.inverse <-
  loadPlotData(name = "moead.norm.inverse", j = id.norm.inverse)
moead.norm.tournament <-
  loadPlotData(name = "moead.norm.tournament", j = id.norm.tournament)

visuEvol(moead.norm, "moead.norm")
visuEvol(moead.norm.inverse, "moead.norm.inverse")
visuEvol(moead.norm.tournament, "moead.norm.tournament")
