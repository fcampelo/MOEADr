load.DTLZ.pareto.front <- function(fun, n.obj) {
  setwd("~/MOEADr/R/")
  fun.number <- as.numeric(strsplit(fun, "DTLZ")[[1]][2])
  if (fun.number == 1) {
    pareto.front <-
      read.csv(paste0("../DTLZ1.", n.obj, "D.pf"),
               header = F,
               sep = '\t')
    pareto.front <- pareto.front[, -c(n.obj + 1)]
    pareto.front <- as.matrix(pareto.front)
    class(pareto.front[, 1])
  } else if (fun.number == 2) {
    pareto.front <-
      read.csv(paste0("../DTLZ2.", n.obj, "D.pf"),
               header = F,
               sep = '\t')
    pareto.front <- pareto.front[, -c(n.obj + 1)]
    pareto.front <- as.matrix(pareto.front)
  } else if (fun.number == 3) {
    if(n.obj == 2){
      pareto.front <-
        read.csv(paste0("../DTLZ3.", n.obj, "D.pf"),
                 header = F,
                 sep = '\t')
    }
    else{
      pareto.front <-
        read.csv(paste0("../DTLZ3.", n.obj, "D.pf"),
                 header = F,
                 sep = ' ')
    }
    pareto.front <- pareto.front[, -c(n.obj + 1)]
    pareto.front <- as.matrix(pareto.front)
  } else if (fun.number == 4) {
    if(n.obj == 2){
      pareto.front <-
        read.csv(paste0("../DTLZ4.", n.obj, "D.pf"),
                 header = F,
                 sep = '\t')
    }
    else{
      pareto.front <-
        read.csv(paste0("../DTLZ4.", n.obj, "D.pf"),
                 header = F,
                 sep = ' ')
    }
    pareto.front <- data.frame(pareto.front[, -c(n.obj + 1)])
    pareto.front <- as.matrix(pareto.front)
  } else if (fun.number == 5) {
    pareto.front <-
      read.csv(paste0("../DTLZ5.", n.obj, "D.pf"),
               header = F,
               sep = ' ')
    pareto.front <- pareto.front[, -c(n.obj + 1)]
    pareto.front <- as.matrix(pareto.front)
  } else if (fun.number == 6) {
    pareto.front <-
      read.csv(paste0("../DTLZ6.", n.obj, "D.pf"),
               header = F,
               sep = ' ')
    pareto.front <- pareto.front[, -c(n.obj + 1)]
    pareto.front <- as.matrix(pareto.front)
  } else if (fun.number == 7) {
    if (n.obj == 2) {
      pareto.front <-
        read.csv(paste0("../DTLZ7.", n.obj, "D.pf"),
                 header = F,
                 sep = '\t')
    }
    else
      pareto.front <-
        read.csv(paste0("../DTLZ7.", n.obj, "D.pf"),
                 header = F,
                 sep = ' ')
    pareto.front <- pareto.front[, -c(n.obj + 1)]
    pareto.front <- data.matrix(pareto.front)
  }
  return(pareto.front)
}
