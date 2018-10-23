library(rcompanion)
library(multcompView)
library(FSA)
library(rcompanion)
library(plyr)

stats <- function(benchmark, data, fun.names, n.obj, ref.points){
  print(benchmark)
  print(n.obj)
  algorithms <- c("moead.de")
  for (algo in algorithms) {
    print(algo)
    algo.data <- data[data$algo == algo, ]
    for (fun in fun.names) {
      print(fun)
      fun.algo.data <- algo.data[algo.data$fun == fun, ]

      # if(kruskal.test(fun.algo.data$X1 ~ fun.algo.data$names)$p.value < 0.05){
        PT = dunnTest(fun.algo.data$X2 ~ fun.algo.data$names,
                      method="bh")    # Can adjust p-values;
        PT = PT$res
        x <-cldList(comparison = PT$Comparison,
                      p.value    = PT$P.adj,
                      threshold  = 0.05)
        fil <- paste("~/Desktop/graphs/data", algo, fun, benchmark,".csv")
        write.csv(x, file = fil)

        write.csv(x, file = fil)
      # }
      # else{print("no stats sig")}
    }
  }


}

fun.names <- list()
for (i in 1:7) {
  fun.names[[length(fun.names) + 1]] = paste0("DTLZ", i)
}
# for (i in 5:7) {
#   fun.names[[length(fun.names) + 1]] = paste0("DTLZ", i)
# }
load("~/MOEADr/R/DTLZ.2o")
load(file = "DTLZ.2o")
data <- DTLZ.2o
n.obj = 2
ref.points <-
  rep(round(1 + 1 / 199, 3), 2)
benchmark = "DTLZ"
stats(benchmark, data, fun.names, n.obj, ref.points)

fun.names <- list()
for (i in 2:3) {
  fun.names[[length(fun.names) + 1]] = paste0("DTLZ", i)
}
for (i in 5:7) {
  fun.names[[length(fun.names) + 1]] = paste0("DTLZ", i)
}
setwd("~/Desktop/graphs")
load(file = "DTLZ.3o")
load(file = "DTLZ.3oSA")
data <- DTLZ.3o
dataRP <- DTLZ.3oSA
n.obj = 3
benchmark = "DTLZ"
ref.points <-
  rep(round(1 + 1 / 99, 3), 3)
dataDTLZ3<-rbind(data, dataRP)
dataDTLZ3$n.obj<-3
stats(benchmark, dataDTLZ3, fun.names, n.obj, ref.points)


setwd("~/Desktop/graphs")
load(file = "WFG.2o")
load(file = "WFG.2o.SA")
data <- WFG.2o
dataRP <- WFG.2o.SA
ref.points <-
  rep(round(1 + 1 / 199, 3), 2)
n.obj = 2
dataWFG2<-rbind(data, dataRP)
dataWFG2$n.obj<-2
fun.names <- list()
for (i in 1:9) {
  fun.names[[length(fun.names) + 1]] = paste0("WFG", i)
}
benchmark = "WFG"
stats(benchmark, dataWFG2, fun.names, n.obj, ref.points)

setwd("~/Desktop/graphs")
load(file = "WFG.3o")
load(file = "WFG.3o.SA")
data <- WFG.3o
dataRP <- WFG.3o.SA
n.obj = 3
dataWFG3<-rbind(data, dataRP)
dataWFG3$n.obj<-3
ref.points <-
  rep(round(1 + 1 / 99, 3), 3)
fun.names <- list()
for (i in 1:9) {
  fun.names[[length(fun.names) + 1]] = paste0("WFG", i)
}
benchmark = "WFG"
stats(benchmark, dataWFG3, fun.names, n.obj, ref.points)

