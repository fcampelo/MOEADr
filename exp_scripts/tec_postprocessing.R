rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADps)
library(emoa)
library(feather)
library(withr)

fun.names1 <- list()
# source("visualization_tools.R")
source("~/MOEADr/R/utils.R")
number.fun <- 1
repetitions <- 10
lambda <- 50

for (i in 1:30) {
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

source("../R/summary_moead.R")

results <- data.frame()
for (fun in fun.names1) {
  ref1 <- data.frame()
  print(fun)

  benchmark <- strsplit(fun, "[0-9]")[[1]][1]
  number <- strsplit(fun, "[A-Z]")[[1]][3]
  if (benchmark == "DTLZ") {
    Yref <-
      as.matrix(read.table(paste0(
        "../inst/extdata/pf_data/", fun, ".2D.pf"
      )))
    colnames(Yref) <- c("f1", "f2")
    ref.point <- c(1, 1)
    # number_subproblems <-
    #   c(3, 4, 6, 8, 10, 30, 50, 100, 150, 250)
  }
  if (benchmark == "UF") {
    Yref <-
      as.matrix(read.table(paste0(
        "../inst/extdata/pf_data/", fun, ".dat"
      )))
    if (as.numeric(number) == 8 ||
        as.numeric(number) == 9 || as.numeric(number) == 10) {
      colnames(Yref) <- c("f1", "f2", "f3")
      ref.point <- c(1, 1, 1)
      # number_subproblems <-
      #   c(4, 6, 8, 10, 30, 50, 100, 150, 250)
    }
    else{
      colnames(Yref) <- c("f1", "f2")
      ref.point <- c(1, 1)
      # number_subproblems <-
      #   c(3, 4, 6, 8, 10, 30, 50, 100, 150, 250)
    }
  }
  else{
    ref.point <- c(1, 1)
    Yref <-
      as.matrix(read.table(paste0("../inst/extdata/pf_data/UF2.dat")))
    colnames(Yref) <- c("f1", "f2")
  }





  for (j in 1:repetitions) {
    # for (lambda in number_subproblems) {
    moead50 <-
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


    moead.ps.50 <-
      loadPlotData(
        name = paste0(fun, "_moead50_", lambda, "_"),
        j = j,
        wd = "~/tec/"
      )
    ref1 <-
      rbind(ref1,
            moead50$Y,
            moead500$Y,
            moead.ps.50$Y)
    # }
  }



  for (j in 1:repetitions) {
    # for (lambda in number_subpCroblems) {
    moead50 <-
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


    moead.ps.50 <-
      loadPlotData(
        name = paste0(fun, "_moead50_", lambda, "_"),
        j = j,
        wd = "~/tec/"
      )

    class(moead50) <- "moead"
    class(moead500) <- "moead"
    class(moead.ps.50) <- "moead"

    small <-
      data.frame(
        summary.moead(
          moead50,
          scaling.reference = ref1,
          ref.point = ref.point,
          ref.front = Yref
        ),
        name = paste0("smallpop_", lambda)
      )

    big <-
      data.frame(
        summary.moead(
          moead500,
          scaling.reference = ref1,
          ref.point = ref.point,
          ref.front = Yref
        ),
        name = paste0("bigpop_", lambda)
      )
    moead.ps.50 <-
      data.frame(
        summary.moead(
          moead.ps.50,
          scaling.reference = ref1,
          ref.point = ref.point,
          ref.front = Yref
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
print(aggregate(results$igd, median, by = list(results$name, results$fun)))
print(aggregate(results$nndom, median, by = list(results$name, results$fun)))
write_feather(results, "collab_results")
