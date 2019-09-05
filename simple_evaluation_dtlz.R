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
number.fun <- 3
repetitions <- 21

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
        "~/MOEADr/inst/extdata/pf_data/", fun, ".2D.pf"
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
  write_feather(ref1, paste0("ref1", fun))
}
print(aggregate(results$hv, median, by = list(results$name, results$fun)))
print(aggregate(results$igd, median, by = list(results$name, results$fun)))
write_feather(results, "results_dtlz")


