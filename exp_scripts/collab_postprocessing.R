
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
repetitions <- 10

for (i in 9:9) {
  fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
}

for (i in 7:7) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i) 
}

number_subproblems <- c(3, 4, 5, 6, 7, 8, 9, 10, 30, 50, 100, 150, 250)

results <- data.frame()
for (fun in fun.names1) {
  ref1 <- data.frame()
  print(fun)
  
  benchmark <- strsplit(fun, "[0-9]")[[1]][1]
  number <- strsplit(fun, "[A-Z]")[[1]][3]
  if (benchmark == "DTLZ"){
    Yref <-
      as.matrix(read.table(paste0(
        "../inst/extdata/pf_data/", fun, ".2D.pf"
      )))
    colnames(Yref) <- c("f1", "f2")
    ref.point <- c(1, 1)
  }
  else {
    Yref <-
      as.matrix(read.table(paste0(
        "../inst/extdata/pf_data/", fun, ".dat"
      )))
    if (as.numeric(number) == 8 || as.numeric(number) == 9 || as.numeric(number) == 10){
      colnames(Yref) <- c("f1", "f2", "f3")
      ref.point <- c(1, 1, 1)
    }
    else{
      colnames(Yref) <- c("f1", "f2")
      ref.point <- c(1, 1)
    }
  }
  
  
  
  
  
  for (j in 1:repetitions) {
    
      for (lambda in number_subproblems) {
      moead.random <-
        loadPlotData(name = paste0(fun, "_moead.random_", lambda, "_", wd = "~/france_data/"),
                     j = j)
      
      
      moead.ds <-
        loadPlotData(name = paste0(fun, "_moead.norm_", lambda, "_", wd = "~/france_data/"),
                     j = j)
      
      
      moead.RI <-
        loadPlotData(name = paste0(fun, "_moead.RI_", lambda, "_", wd = "~/france_data/"),
                     j = j)
      ref1 <-
        rbind(
          ref1,
          moead.random$Y,
          moead.ds$Y,
          moead.RI$Y
        )
      }
    }
  
  
  
  for (j in 1:repetitions) {
    for (lambda in number_subproblems) {
      moead.random <-
        loadPlotData(name = paste0(fun, "_moead.random_", lambda, "_", wd = "~/france_data/"),
                     j = j)
      
      
      moead.ds <-
        loadPlotData(name = paste0(fun, "_moead.norm_", lambda, "_", wd = "~/france_data/"),
                     j = j)
      
      
      moead.RI <-
        loadPlotData(name = paste0(fun, "_moead.RI_", lambda, "_", wd = "~/france_data/"),
                     j = j)
    
    
    class(moead.random) <- "moead"
    class(moead.ds) <- "moead"
    class(moead.RI) <- "moead"
    
    random <-
      data.frame(
        summary(
          moead.random,
          scaling.reference = ref1,
          ref.point = ref.point
        ),
        name = paste0("random_", lambda)
      )
    
    DS <-
      data.frame(
        summary(
          moead.ds,
          scaling.reference = ref1,
          ref.point = ref.point
        ),
        name = paste0("DS_", lambda)
      )
    RI <-
      data.frame(
        summary(
          moead.RI,
          scaling.reference = ref1,
          ref.point = ref.point
        ),
        name = paste0("RI_", lambda)
      )
    
    results <-
      rbind(results, cbind(
        rbind(
          random,
          DS,
          RI
        ),
        fun
      ))
    
    }
    
  }
}
print(aggregate(results$hv, median, by = list(results$name, results$fun)))
print(aggregate(results$igd, median, by = list(results$name, results$fun)))
print(aggregate(results$nndom, median, by = list(results$name, results$fun)))
write_feather(results, "collab_results")

