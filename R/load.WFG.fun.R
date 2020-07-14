
load.WFG.function <- function(fun, n.obj = 2) {
  if (fun == "WFG1") {
    wfg1 = makeWFG1Function(n.objectives = n.obj,)
    problem.wfg = wfg1
  } else if (fun == "WFG2") {
    wfg2 = makeWFG2Function(n.objectives = n.obj,)
    problem.wfg = wfg2
  } else if (fun == "WFG3") {
    wfg3 = makeWFG3Function(n.objectives = n.obj)
    problem.wfg = wfg3
  } else if (fun == "WFG4") {
    wfg4 = makeWFG4Function(n.objectives = n.obj)
    problem.wfg = wfg4
  } else if (fun == "WFG5") {
    wfg5 = makeWFG5Function(n.objectives = n.obj)
    problem.wfg = wfg5
  } else if (fun == "WFG6") {
    wfg6 = makeWFG6Function(n.objectives = n.obj)
    problem.wfg = wfg6
  } else if (fun == "WFG7") {
    wfg7 = makeWFG7Function(n.objectives = n.obj)
    problem.wfg = wfg7
  } else if (fun == "WFG8") {
    wfg8 = makeWFG8Function(n.objectives = n.obj)
    problem.wfg = wfg8
  } else if (fun == "WFG9") {
    wfg9 = makeWFG9Function(n.objectives = n.obj)
    problem.wfg = wfg9
  }
  out <- list(fn = problem.wfg)
  return(out)
}
