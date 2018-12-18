load.DTLZ.function <- function(fun, dimensions, n.obj) {
  if (fun == "DTLZ1") {
    dtlz1 = makeDTLZ1Function(n.objectives = n.obj, dimensions = dimensions
    )
    problem.dtlz = dtlz1
  } else if (fun == "DTLZ2") {
    dtlz2 = makeDTLZ2Function(n.objectives = n.obj, dimensions = dimensions
                              
    )
    problem.dtlz = dtlz2
  } else if (fun == "DTLZ3") {
    dtlz3 = makeDTLZ3Function(n.objectives = n.obj, dimensions = dimensions
                              
    )
    problem.dtlz = dtlz3
  } else if (fun == "DTLZ4") {
    dtlz4 = makeDTLZ4Function(n.objectives = n.obj, dimensions = dimensions
                              
    )
    problem.dtlz = dtlz4
  } else if (fun == "DTLZ5") {
    dtlz5 = makeDTLZ5Function(n.objectives = n.obj, dimensions = dimensions
                              
    )
    problem.dtlz = dtlz5
  } else if (fun == "DTLZ6") {
    dtlz6 = makeDTLZ6Function(n.objectives = n.obj, dimensions = dimensions
                              
    )
    problem.dtlz = dtlz6
  } else if (fun == "DTLZ7") {
    dtlz7 = makeDTLZ7Function(n.objectives = n.obj, dimensions = dimensions
                              
    )
    problem.dtlz = dtlz7
  }
  out <- list(fn = problem.dtlz)
  return(out)
}
