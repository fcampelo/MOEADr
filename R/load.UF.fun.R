load.UF.function <- function(fun, d){
  if (fun == "UF1") {
    uf1 = makeUFFunction(id = 1,dimensions = d)
    problem.uf = uf1
  }
  else if (fun == "UF2") {
    uf2 = makeUFFunction(id = 2,dimensions = d)
    problem.uf = uf2
  }else if (fun == "F3") {
    uf3 = makeUFFunction(id = 3,dimensions = d)
    problem.uf = uf3
  }
  else if (fun == "UF4") {
    uf4 = makeUFFunction(id = 4,dimensions = d)
    problem.uf = uf4
  }
  else if (fun == "UF5") {
    uf5 = makeUFFunction(id = 5,dimensions = d)
    problem.uf = uf5
  }
  else if (fun == "UF") {
    uf6 = makeUFFunction(id = 6,dimensions = d)
    problem.uf = uf6
  }
  else if (fun == "UF7") {
    uf7 = makeUFFunction(id = 7,dimensions = d)
    problem.uf = uf7
  }
  out <- list(fn = problem.uf)
  return(out)
}
