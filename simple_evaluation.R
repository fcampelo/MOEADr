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
    moead.random <-
      loadPlotData(name = paste0(fun, "moead.random"),
                   j = i)
    moead.R.I. <-
      loadPlotData(name = paste0(fun, "moead.gra"),
                   j = i)
    
    ref1 <-
      rbind(ref1, moead.random$Y,
            moead.R.I.$Y,
            moead.norm.inverse$Y,
            moead.de$Y,
            moead.norm$Y)
  }
  
  for (i in 1:repetitions) {
    moead.de <- loadPlotData(name = paste0(fun, "moead.de"), j = i)
    moead.norm <- loadPlotData(name = paste0(fun, "moead.norm"), j = i)
    moead.norm.inverse <-
      loadPlotData(name = paste0(fun, "moead.norm.inverse"), j = i)
    moead.random <-
      loadPlotData(name = paste0(fun, "moead.random"),
                   j = i)
    moead.R.I. <-
      loadPlotData(name = paste0(fun, "moead.gra"),
                   j = i)
    
    class(moead.de) <- "moead"
    class(moead.norm) <- "moead"
    class(moead.norm.inverse) <- "moead"
    class(moead.random) <- "moead"
    class(moead.R.I.) <- "moead"
    
    Yref <-
      as.matrix(read.table(paste0(
        "../inst/extdata/pf_data/", fun, ".2D.pf"
      )))
    colnames(Yref) <- c("f1", "f2")
  
    de <-
      data.frame(summary(
        moead.de,
        scaling.reference = ref1,
        ref.point = c(1, 1),
        ref.front = Yref,
      ), name = "de.data")
    norm <-
      data.frame(summary(
        moead.norm,
        scaling.reference = ref1,
        ref.point = c(1, 1),
        ref.front = Yref,
      ), name = "norm.data")
    norm.inverse <-
      data.frame(summary(
        moead.norm.inverse,
        scaling.reference = ref1,
        ref.point = c(1, 1),
        ref.front = Yref
      ),
      name = "norm.inverse.data")
    random <-
      data.frame(summary(
        moead.random,
        scaling.reference = ref1,
        ref.point = c(1, 1),
        ref.front = Yref
      ),
      name = "random.data")
    R.I. <-
      data.frame(summary(
        moead.R.I.,
        scaling.reference = ref1,
        ref.point = c(1, 1),
        ref.front = Yref
      ),
      name = "R.I.data")
  }
  results <- rbind(results, cbind(rbind(de, norm, norm.inverse, random, R.I.), fun))
#print(results)
#exit()
}
print(aggregate(results$hv, median, by = list(results$name, results$fun)))
print(aggregate(results$igd, median, by = list(results$name, results$fun)))
write_feather(results, "results_dtlz")

####
for (fun in fun.names1) {

  
  # agg.igd <- aggregate(results$igd, median, by = list(results$name, results$fun))
  temp.results <- results[results$fun == fun, ]
  agg.hv <- aggregate(temp.results$hv, median, by = list(temp.results$name, temp.results$fun))
  id.de <-
    which(temp.results[temp.results$name == "de.data", ]$hv == agg.hv[agg.hv$Group.1 ==
                                                              "de.data", ]$x)
  id.norm <-
    which(temp.results[temp.results$name == "norm.data", ]$hv == agg.hv[agg.hv$Group.1 ==
                                                                "norm.data", ]$x)
  id.norm.inverse <-
    which(temp.results[temp.results$name == "norm.inverse.data", ]$hv == agg.hv[agg.hv$Group.1 ==
                                                                        "norm.inverse.data", ]$x)
  id.R.I. <-
    which(temp.results[temp.results$name == "R.I.data", ]$hv == agg.hv[agg.hv$Group.1 ==
                                                                      "R.I.data", ]$x)
  random <-
    which(temp.results[temp.results$name == "random.data", ]$hv == agg.hv[agg.hv$Group.1 ==
                                                                      "random.data", ]$x)
  
  
  
moead.de <- loadPlotData(name = paste0(fun, "moead.de"), j = id.de)
moead.norm <- loadPlotData(name = paste0(fun, "moead.norm"), j = id.norm)
moead.norm.inverse <-
  loadPlotData(name = paste0(fun, "moead.norm.inverse"), j = id.norm.inverse)
moead.random <-
  loadPlotData(name = paste0(fun, "moead.random"), j = id.norm.inverse)
moead.R.I. <-
  loadPlotData(name = paste0(fun, "moead.gra"), j = id.norm.inverse)


p <- visuEvol(moead.de, "MOEA/D-DE - No Resource Allocation", fun, ref1)
file <- paste0("~/MOEADr/dataExp/visu/", fun, "_visual_", "MOEAD_DE_None", ".html")
saveWidgetFix(p, file)

p <- visuEvol(moead.norm, "MOEA/D-DE - Norm Resource Allocation", fun, ref1)
file <- paste0("~/MOEADr/dataExp/visu/", fun, "_visual_", "MOEAD_DE_Norm", ".html")
saveWidgetFix(p, file)

p <- visuEvol(moead.norm.inverse, "MOEA/D-DE - (1-Norm) Resource Allocation", fun, ref1)
file <- paste0("~/MOEADr/dataExp/visu/", fun, "_visual_", "MOEAD_DE_InverseNorm", ".html")
saveWidgetFix(p, file)

p <- visuEvol(moead.random, "MOEA/D-DE - Random Resource Allocation", fun, ref1)
file <- paste0("~/MOEADr/dataExp/visu/", fun, "_visual_", "MOEAD_DE_Random", ".html")
saveWidgetFix(p, file)

p <- visuEvol(moead.R.I., "MOEA/D-DE - R.I. Resource Allocation", fun, ref1)
file <- paste0("~/MOEADr/dataExp/visu/", fun, "_visual_", "MOEAD_DE_RI", ".html")
saveWidgetFix(p, file)

}
