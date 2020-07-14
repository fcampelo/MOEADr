load.WFG.pareto.front <- function(fun, n.obj) {
  setwd("~/MOEADr/R/")
  if (fun == "WFG1") {
    print(paste0("../WFG1.", n.obj, "D.pf"))
    pareto.front <-
      read.csv(paste0("../WFG1.", n.obj, "D.pf"),
               header = F,
               sep = ' ')
    pareto.front <- pareto.front[, -c(n.obj + 1)]
    pareto.front <- as.matrix(pareto.front)
  } else if (fun == "WFG2") {
    if(n.obj == 2){
      pareto.front <-
        read.csv(paste0("../WFG2.", n.obj, "D.pf"),
                 header = F,
                 sep = '\t')
    }
    else{
      pareto.front <-
        read.csv(paste0("../WFG2.", n.obj, "D.pf"),
                 header = F,
                 sep = ' ')
    }
    pareto.front <- pareto.front[, -c(n.obj + 1)]
    pareto.front <- as.matrix(pareto.front)
  } else if (fun == "WFG3") {
    pareto.front <-
      read.csv(paste0("../WFG3.", n.obj, "D.pf"),
               header = F,
               sep = ' ')
    pareto.front <- pareto.front[, -c(n.obj + 1)]
    pareto.front <- as.matrix(pareto.front)
  } else if (fun == "WFG4") {
    pareto.front <-
      read.csv(paste0("../WFG4.", n.obj, "D.pf"),
               header = F,
               sep = ' ')
    pareto.front <- pareto.front[, -c(n.obj + 1)]
    pareto.front <- as.matrix(pareto.front)
  } else if (fun == "WFG5") {
    pareto.front <-
      read.csv(paste0("../WFG5.", n.obj, "D.pf"),
               header = F,
               sep = ' ')
    pareto.front <- pareto.front[, -c(n.obj + 1)]
    pareto.front <- as.matrix(pareto.front)
  } else if (fun == "WFG6") {
    pareto.front <-
      read.csv(paste0("../WFG6.", n.obj, "D.pf"),
               header = F,
               sep = ' ')
    pareto.front <- pareto.front[, -c(n.obj + 1)]
    pareto.front <- as.matrix(pareto.front)
  } else if (fun == "WFG7") {
    pareto.front <-
      read.csv(paste0("../WFG7.", n.obj, "D.pf"),
               header = F,
               sep = ' ')
    pareto.front <- pareto.front[, -c(n.obj + 1)]
    pareto.front <- as.matrix(pareto.front)
  }
  else if (fun == "WFG8") {
    pareto.front <-
      read.csv(paste0("../WFG8.", n.obj, "D.pf"),
               header = F,
               sep = ' ')
    pareto.front <- pareto.front[, -c(n.obj + 1)]
    pareto.front <- as.matrix(pareto.front)
  } else if (fun == "WFG9") {
    pareto.front <-
      read.csv(paste0("../WFG9.", n.obj, "D.pf"),
               header = F,
               sep = ' ')
    pareto.front <- pareto.front[, -c(n.obj + 1)]
    pareto.front <- as.matrix(pareto.front)
  }
  return(pareto.front)
}
