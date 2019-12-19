# source('~/myMOEADr/exp_UF10_0.2_0.4.R')
# source('~/myMOEADr/exp_DTLZ7_0.2_0.4.R')
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
repetitions <- 21

for (i in 1:10) {
    fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
}

for (i in 1:7) {
   fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i) 
}



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
        moead.random_fixed.0.1 <-
            loadPlotData(name = paste0(fun, "moead.random_fixed.0.1"),
                         j = j)
        moead.random_fixed.0.2 <-
            loadPlotData(name = paste0(fun, "moead.random_fixed.0.2"),
                         j = j)
        
        moead.random_fixed.0.4 <-
            loadPlotData(name = paste0(fun, "moead.random_fixed.0.4"),
                         j = j)
        
        moead.random_fixed.0.6 <-
            loadPlotData(name = paste0(fun, "moead.random_fixed.0.6"),
                         j = j)
        
        moead.random_fixed.0.8 <-
            loadPlotData(name = paste0(fun, "moead.random_fixed.0.8"),
                         j = j)
        
        moead.random_fixed.1.0 <-
            loadPlotData(name = paste0(fun, "moead.random_fixed.1.0"),
                         j = j)
        
        moead.ds <-
            loadPlotData(name = paste0(fun, "moead.norm"),
                         j = j)
        
        moead.RI <-
            loadPlotData(name = paste0(fun, "moead.RI"),
                         j = j)
        ref1 <-
            rbind(
                ref1,
                moead.random_fixed.0.1$Y,
                moead.random_fixed.1.0$Y,
                moead.random_fixed.0.8$Y,
                moead.random_fixed.0.6$Y,
                moead.random_fixed.0.4$Y,
                moead.random_fixed.0.2$Y,
                moead.ds$Y,
                moead.RI$Y
            )
    }
    for (j in 1:repetitions) {
        moead.random_fixed.0.1 <-
            loadPlotData(name = paste0(fun, "moead.random_fixed.0.1"),
                         j = j)
        moead.random_fixed.0.2 <-
            loadPlotData(name = paste0(fun, "moead.random_fixed.0.2"),
                         j = j)
        moead.random_fixed.0.4 <-
            loadPlotData(name = paste0(fun, "moead.random_fixed.0.4"),
                         j = j)
        moead.random_fixed.0.6 <-
            loadPlotData(name = paste0(fun, "moead.random_fixed.0.6"),
                         j = j)
        moead.random_fixed.0.8 <-
            loadPlotData(name = paste0(fun, "moead.random_fixed.0.8"),
                         j = j)
        moead.random_fixed.1.0 <-
            loadPlotData(name = paste0(fun, "moead.random_fixed.1.0"),
                         j = j)
        moead.ds <-
            loadPlotData(name = paste0(fun, "moead.norm"),
                         j = j)
        moead.RI <-
            loadPlotData(name = paste0(fun, "moead.RI"),
                         j = j)
        
        
        class(moead.random_fixed.0.1) <- "moead"
        class(moead.random_fixed.0.2) <- "moead"
        class(moead.random_fixed.0.4) <- "moead"
        class(moead.random_fixed.0.6) <- "moead"
        class(moead.random_fixed.0.8) <- "moead"
        class(moead.random_fixed.1.0) <- "moead"
        class(moead.ds) <- "moead"
        class(moead.RI) <- "moead"
        
        fixed.0.1 <-
            data.frame(
                summary(
                    moead.random_fixed.0.1,
                    scaling.reference = ref1,
                    ref.point = ref.point,
                    ref.front = Yref,
                ),
                name = "moead.random_fixed.0.1"
            )
        fixed.0.2 <-
            data.frame(
                summary(
                    moead.random_fixed.0.2,
                    scaling.reference = ref1,
                    ref.point = ref.point,
                    ref.front = Yref,
                ),
                name = "moead.random_fixed.0.2"
            )
        fixed.0.4 <-
            data.frame(
                summary(
                    moead.random_fixed.0.4,
                    scaling.reference = ref1,
                    ref.point = ref.point,
                    ref.front = Yref,
                ),
                name = "moead.random_fixed.0.4"
            )
        fixed.0.6 <-
            data.frame(
                summary(
                    moead.random_fixed.0.6,
                    scaling.reference = ref1,
                    ref.point = ref.point,
                    ref.front = Yref,
                ),
                name = "moead.random_fixed.0.6"
            )
        fixed.0.8 <-
            data.frame(
                summary(
                    moead.random_fixed.0.8,
                    scaling.reference = ref1,
                    ref.point = ref.point,
                    ref.front = Yref,
                ),
                name = "moead.random_fixed.0.8"
            )
        fixed.1.0 <-
            data.frame(
                summary(
                    moead.random_fixed.1.0,
                    scaling.reference = ref1,
                    ref.point = ref.point,
                    ref.front = Yref
                ),
                name = "moead.random_fixed.1.0"
            )
        DS <-
            data.frame(
                summary(
                    moead.ds,
                    scaling.reference = ref1,
                    ref.point = ref.point,
                    ref.front = Yref
                ),
                name = "DS"
            )
        RI <-
            data.frame(
                summary(
                    moead.RI,
                    scaling.reference = ref1,
                    ref.point = ref.point,
                    ref.front = Yref
                ),
                name = "RI"
            )
        
        results <-
            rbind(results, cbind(
                rbind(
                    fixed.0.1,
                    fixed.0.2,
                    fixed.0.4,
                    fixed.0.6,
                    fixed.0.8,
                    fixed.1.0,
                    DS,
                    RI
                ),
                fun
            ))
        
        
    }
}
print(aggregate(results$hv, median, by = list(results$name, results$fun)))
print(aggregate(results$igd, median, by = list(results$name, results$fun)))
write_feather(results, "results_fixed")

