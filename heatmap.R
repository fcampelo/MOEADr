data <- rbind(dataWFG3, dataDTLZ3)
N=10
c1 <- 0
c2 <- 0
medians <- rep(0, N)
max.median <- rep(0, N)
my.data <- data.frame(matrix(ncol = 2, nrow = 10))
x <- c("rank", "fun")
colnames(my.data) <- x
values.median<-matrix(nrow = 16, ncol=10, 0)

names <- c("betnrun", "control", "small.pop", "betnrun.RP", "small.pop.RP")
names <- levels(fun.algo.data$names)
names(max.median) <- names
fun.names <- list()
for (i in 1:9) {
  fun.names[[length(fun.names) + 1]] = paste0("WFG", i)
}
for (i in 1:7) {
  fun.names[[length(fun.names) + 1]] = paste0("DTLZ", i)
}
algorithms <- c("moead.de", "original")
# algorithms <- c("moead.de")

for (algo in algorithms) {
  values.median<-matrix(nrow = 16, ncol=10, 0)
  names <- c("betnrun", "control", "small.pop", "betnrun.RP", "small.pop.RP")
  names <- levels(fun.algo.data$names)
  names(max.median) <- names
  j <- 1
  algo.data <- data[data$algo == algo, ]
  my.mean  <- mean(algo.data$X1)
  for (fun in fun.names) {
    print(fun)
    fun.algo.data <- algo.data[algo.data$fun == fun, ]
    i <- 1

    for (name in names) {
      medians[i] <-
        median(fun.algo.data[fun.algo.data$names == name,]$X1)
      if (name == "control"){
        name <- algo}
      names(medians)[i] <- c(name)
      i = i + 1
    }
    temp <- rep(0,N)
    aux <- medians

    for (i in 1:N){
      temp[which(medians==aux[which.max(aux)])] <- i
      aux <- aux[-c(which.max(aux))]
    }
    if (!is.na(my.data$rank[1])){
      my.data$rank <- my.data$rank + temp
    }else{
      my.data$rank <- temp
      my.data$fun <- names
    }

    values.median[j,]<-unname(medians)-my.mean
    # # colnames(values.median[,j]) <- c("fun")
    # rownames(values.median[j,]) <- c("fun")
    j=j+1

  }
  colnames(values.median) <- names
  rownames(values.median) <- c("WFG1",
                               "WFG2",
                               "WFG3",
                               "WFG4",
                               "WFG5",
                               "WFG6",
                               "WFG7",
                               "WFG8",
                               "WFG9",
                               "DTLZ1",
                               "DTLZ2",
                               "DTLZ3",
                               "DTLZ4",
                               "DTLZ5",
                               "DTLZ6",
                               "DTLZ7")

    heatmap(t(values.median), ylab = "fun", xlab = "algo", main = algo, margins = c(6, 10))


}
# print(my.data)
# print(kruskal.test(my.data$rank ~ my.data$fun))
# print(pairwise.wilcox.test(my.data$rank, my.data$fun,
#                            p.adjust.method = "BH"))



