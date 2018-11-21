setwd("~/MOEADr/")

create_graphs <-
  function(benchmark,
           data,
           fun.names,
           n.obj,
           ref.points) {
    aux <- data[data$base.algorithm == "moead.de",]
    for (fun in fun.names) {
    # aux1 <- aux[aux$fun == fun, ]
    pathname <- paste0("HV/moead.de_", fun, ".png")
    png(pathname,
        width = 1000,
        height = 600)
    par(cex.axis = 1.5)
    boxplot(
      my.data[my.data$fun == fun,]$HV ~ my.data[my.data$fun == fun,]$variation.name,
      col = c("blue", "gray", "orange", "green", "brown"),
      las = 1
    )
    dev.off()
    }
  }



load(file = "R/BiObjBBOB7_2")
# my.data <- my.data[my.data$variation.name != "MOEA/D-DRA", ]
# my.data <- my.data[my.data$variation.name != "NSGA-2", ]

my.data$variation.name <-
  factor(levels(my.data$variation.name)[c(-2, -5)])
data <- my.data
fun.names <- list()
for (i in 1:7) {
  fun.names[[length(fun.names) + 1]] = paste0("BiObjBBOB", i)
}
n.obj <- 2
benchmark = "BiObjBBOB"
ref.points <- rep(round(1 + 1 / 19, 3), 3)
create_graphs(benchmark, data, fun.names, n.obj, ref.points)
