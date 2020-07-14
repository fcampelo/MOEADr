setwd("~/MOEADr/R")
rm(list = ls(all = TRUE))
library(R.utils)
library(moobench)
sourceDirectory(".")
file.sources = list.files(pattern = "*.R")
setwd("~/Desktop/graphs/")
create_graphs <- function(benchmark, data, data01, fun.names, n.obj, ref.points){
  algorithms <- c("moead.de", "original")
  for (algo in algorithms) {
    aux <- data[data$algo == algo, ]
    aux01 <- data01[data01$algo == algo, ]
    for (fun in fun.names) {
      aux1 <- aux[aux$fun == fun, ]
      aux001 <- aux01[aux01$fun == fun, ]
      max.aux <- (max(aux1$X1))
      max.aux01 <- (max(aux001$X1))
      if (max.aux >= max.aux01) max.t <- max.aux
      if (max.aux >= max.aux01) max.t <- max.aux01
      if (benchmark == "DTLZ") ylim = c(0, max.aux*1.5)
      else ylim = c(0, max.aux*1.5)

      pathname <- paste0("HV/images-",n.obj,"O-",benchmark,"/", algo, "_", fun, ".png")


      png(pathname,
          width = 1000,
          height = 600)
      par(mfrow = c(1, 2))
      boxplot(
        aux1$X1 ~ aux1$names,
        main = paste("default.par", algo, fun),
        ylim = ylim,
        las = 2,
        par(mar = c(16, 5, 4, 2) + 0.1)
      )
      boxplot(
        aux001$X1 ~ aux001$names,
        main = paste("random.par", algo, fun),
        ylim = ylim,
        las = 2,
        par(mar = c(16, 5, 4, 2) + 0.1)
      )

      dev.off()
    }
  }

}


load(file = "DTLZ.2oSA")
load(file = "DTLZ.2o")
str(DTLZ.2o)
data <- DTLZ.2o
data01 <- DTLZ.2oSA
n.obj <- 2
fun.names <- list()
for (i in 1:7) {
  fun.names[[length(fun.names) + 1]] = paste0("DTLZ", i)
}
benchmark = "DTLZ"
ref.points <- rep(round(1 + 1 / 199, 3), 2)
create_graphs(benchmark,data, data01, fun.names, n.obj, ref.points)

load(file = "DTLZ.3o")
load(file = "DTLZ.3oSA")
str(DTLZ.3o)
data <- DTLZ.3o
data01 <- DTLZ.3oSA
fun.names <- list()
n.obj <- 3
for (i in 1:7) {
  fun.names[[length(fun.names) + 1]] = paste0("DTLZ", i)
}
benchmark = "DTLZ"
ref.points <- rep(round(1 + 1 / 19, 3), 3)
create_graphs(benchmark,data, data01, fun.names, n.obj, ref.points)

load(file = "WFG.2o")
load(file = "WFG.2o.SA")
n.obj = 2
data <- WFG.2o
data01 <- WFG.2o.SA
ref.points <-
  rep(round(1 + 1 / 199, 3), 2)
fun.names <- list()
for (i in 1:9) {
  fun.names[[length(fun.names) + 1]] = paste0("WFG", i)
}
benchmark = "WFG"
create_graphs(benchmark,data, data01, fun.names, n.obj, ref.points)

load(file = "WFG.3o")
load(file = "WFG.3o.SA")
data <- WFG.3o
data01 <- WFG.3o.SA
fun.names <- list()
for (i in 1:9) {
  fun.names[[length(fun.names) + 1]] = paste0("WFG", i)
}
n.obj <- 3
benchmark = "WFG"
ref.points <- rep(round(1 + 1 / 19, 3), 3)
create_graphs(benchmark,data, data01, fun.names, n.obj, ref.points)



