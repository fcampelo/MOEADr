rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADps)
library(emoa)
library(feather)
library(withr)

fun.names1 <- list()
source("~/MOEADr/R/summary_moeadps.R")
source("~/MOEADr/R/utils.R")
repetitions <- 10
lambda <- 50

for (i in 1:55) {
  fun.names1[[length(fun.names1) + 1]] = paste0("bibbob_", i)
}

for (i in 1:10) {
  fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
}

for (i in 1:7) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}

for (i in 1:4) {
  fun.names1[[length(fun.names1) + 1]] = paste0("inv_DTLZ", i)
}

#number_subproblems <- c(3, 4, 5, 6, 7, 8, 9, 10, 30, 50, 100, 150, 250)


results <- data.frame()
for (fun in fun.names1) {
  ref1 <- data.frame()
  print(fun)
  
  benchmark <- strsplit(fun, "[0-9]")[[1]][1]
  number <- strsplit(fun, "[A-Z]")[[1]][3]
  
  if (benchmark == "DTLZ") {
    # Yref <-
    #   as.matrix(read.table(paste0(
    #     "../inst/extdata/pf_data/", fun, ".2D.pf"
    #   )))
    # colnames(Yref) <- c("f1", "f2")
    # ref.point <- c(1, 1)
  }
  if (benchmark == "UF") {
    # Yref <-
    #   as.matrix(read.table(paste0(
    #     "../inst/extdata/pf_data/", fun, ".dat"
    #   )))
    if (as.numeric(number) == 8 ||
        as.numeric(number) == 9 || as.numeric(number) == 10) {
      # colnames(Yref) <- c("f1", "f2", "f3")
      ref.point <- c(1, 1, 1)
    }
    else{
      # colnames(Yref) <- c("f1", "f2")
      ref.point <- c(1, 1)
    }
  }
  else{
    ref.point <- c(1, 1)
    # Yref <-
    #   as.matrix(read.table(paste0("../inst/extdata/pf_data/UF2.dat")))
    # colnames(Yref) <- c("f1", "f2")
  }
  
  
  
  
  
  for (j in 1:repetitions) {
    # for (lambda in number_subproblems) {
    moead.ps.50 <-
      loadPlotData(
        name = paste0(fun, "_moead.ps.50_", lambda, "_"),
        j = j,
        wd = "~/tec/"
      )
    
    
    moead500 <-
      loadPlotData(
        name = paste0(fun, "_moead500_", lambda, "_"),
        j = j,
        wd = "~/tec/"
      )
    
    
    moead50 <-
      loadPlotData(
        name = paste0(fun, "_moead50_", lambda, "_"),
        j = j,
        wd = "~/tec/"
      )
    moead50$n.iter <- as.integer(moead50$n.iter)
    iter.max <- max(moead50$n.iter)
    
    if (benchmark == "UF") {
      if (number >= 8) {
        moead50$Y <-
          rbind(
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max,]$V3
            ),
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 1,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 1,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 1,]$V3
            ),
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 2,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 2,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 2,]$V3
            ),
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 3,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 3,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 3,]$V3
            ),
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 4,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 4,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 4,]$V3
            ),
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 5,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 5,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 5,]$V3
            ),
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 6,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 6,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 6,]$V3
            ),
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 7,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 7,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 7,]$V3
            ),
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 8,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 8,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 8,]$V3
            ),
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 9,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 9,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 9,]$V3
            )
          )
      }
    } else{
      moead50$Y <-
        rbind(
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max,]$V2
          ),
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 1,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 1,]$V2
          ),
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 2,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 2,]$V2
          ),
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 3,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 3,]$V2
          ),
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 4,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 4,]$V2
          ),
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 5,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 5,]$V2
          ),
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 6,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 6,]$V2
          ),
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 7,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 7,]$V2
          ),
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 8,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 8,]$V2
          ),
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 9,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 9,]$V2
          )
        )
    }
    colnames(moead50$Y) <- colnames(moead500$Y)
    
    ref1 <-
      rbind(ref1,
            moead50$Y,
            moead500$Y,
            moead.ps.50$Y)
    # }
  }
  
  
  
  for (j in 1:repetitions) {
    # for (lambda in number_subpCroblems) {
    
    moead500 <-
      loadPlotData(
        name = paste0(fun, "_moead500_", lambda, "_"),
        j = j,
        wd = "~/tec/"
      )
    
    moead50 <-
      loadPlotData(
        name = paste0(fun, "_moead50_", lambda, "_"),
        j = j,
        wd = "~/tec/"
      )
    moead50$n.iter <- as.integer(moead50$n.iter)
    iter.max <- max(moead50$n.iter)
    if (benchmark == "UF") {
      if (number >= 8) {
        moead50$Y <-
          rbind(
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max,]$V3
            ),
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 1,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 1,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 1,]$V3
            ),
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 2,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 2,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 2,]$V3
            ),
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 3,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 3,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 3,]$V3
            ),
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 4,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 4,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 4,]$V3
            ),
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 5,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 5,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 5,]$V3
            ),
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 6,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 6,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 6,]$V3
            ),
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 7,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 7,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 7,]$V3
            ),
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 8,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 8,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 8,]$V3
            ),
            cbind(
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 9,]$V1,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 9,]$V2,
              moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 9,]$V3
            )
          )
      }
    } else{
      moead50$Y <-
        rbind(
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max,]$V2
          ),
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 1,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 1,]$V2
          ),
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 2,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 2,]$V2
          ),
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 3,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 3,]$V2
          ),
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 4,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 4,]$V2
          ),
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 5,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 5,]$V2
          ),
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 6,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 6,]$V2
          ),
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 7,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 7,]$V2
          ),
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 8,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 8,]$V2
          ),
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 9,]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == iter.max - 9,]$V2
          )
        )
    }
    colnames(moead50$Y) <- colnames(moead500$Y)
    
    
    
    moead.ps.50 <-
      loadPlotData(
        name = paste0(fun, "_moead.ps.50_", lambda, "_"),
        j = j,
        wd = "~/tec/"
      )
    
    class(moead50) <- "moead"
    class(moead500) <- "moead"
    class(moead.ps.50) <- "moead"
    
    small <-
      data.frame(
        summary.moeadps(
          moead50,
          scaling.reference = ref1,
          ref.point = ref.point#,
          # ref.front = Yref
        ),
        name = paste0("smallpop_", lambda)
      )
    
    big <-
      data.frame(
        summary.moeadps(
          moead500,
          scaling.reference = ref1,
          ref.point = ref.point#,
          # ref.front = Yref
        ),
        name = paste0("bigpop_", lambda)
      )
    moead.ps.50 <-
      data.frame(
        summary.moeadps(
          moead.ps.50,
          scaling.reference = ref1,
          ref.point = ref.point#,
          # ref.front = Yref
        ),
        name = paste0("ps_50_", lambda)
      )
    
    results <-
      rbind(results, cbind(rbind(small,
                                 big,
                                 moead.ps.50),
                           fun))
    
    # }
    
  }
}
print(aggregate(results$hv, median, by = list(results$name, results$fun)))
# print(aggregate(results$igd, median, by = list(results$name, results$fun)))
print(aggregate(results$nndom, median, by = list(results$name, results$fun)))
write_feather(results, "~/tec/tec_results")
