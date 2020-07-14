load.UF.function <- function(fun, n.obj){
  if (fun == "UF1"){
    UF1 = generateUF(1, in.dim = d, out.dim = n.obj)
    pf = getParetoFront(UF1, 10000)
    problem.UF <- function(X) {
      t(apply(X, MARGIN = 1,
              FUN = UF1
      ))
    }
  }else if (fun == "UF2"){
    UF2 = generateUF(2, in.dim = d, out.dim = n.obj)
    pf = getParetoFront(UF2, 10000)
    problem.UF <- function(X) {
      t(apply(X, MARGIN = 1,
              FUN = UF2
      ))
    }
  }else if (fun == "UF3"){
    UF3 = generateUF(3, in.dim = d, out.dim = n.obj)
    pf = getParetoFront(UF3, 10000)
    problem.UF <- function(X) {
      t(apply(X, MARGIN = 1,
              FUN = UF3
      ))
    }
  }else if (fun == "UF4"){
    UF4 = generateUF(4, in.dim = d, out.dim = n.obj)
    pf = getParetoFront(UF4, 10000)
    problem.UF <- function(X) {
      t(apply(X, MARGIN = 1,
              FUN = UF4
      ))
    }
  }else if (fun == "UF5"){
    UF5 = generateUF(5, in.dim = d, out.dim = n.obj)
    pf = getParetoFront(UF5, 10000)
    problem.UF <- function(X) {
      t(apply(X, MARGIN = 1,
              FUN = UF5
      ))
    }
  }else if (fun == "UF6"){
    UF6 = generateUF(6, in.dim = d, out.dim = n.obj)
    pf = getParetoFront(UF6, 10000)
    problem.UF <- function(X) {
      t(apply(X, MARGIN = 1,
              FUN = UF6
      ))
    }
  }else if (fun == "UF7"){
    UF7 = generateUF(7, in.dim = d, out.dim = n.obj)
    pf = getParetoFront(UF7, 10000)
    problem.UF <- function(X) {
      t(apply(X, MARGIN = 1,
              FUN = UF7
      ))
    }
  }
  else if (fun == "UF8"){
    UF8 = generateUF(8, in.dim = d, out.dim = n.obj)
    pf = getParetoFront(UF8, 10000)
    problem.UF <- function(X) {
      t(apply(X, MARGIN = 1,
              FUN = UF8
      ))
    }
  }else if (fun == "UF9"){
    UF9 = generateUF(9, in.dim = d, out.dim = n.obj)
    pf = getParetoFront(UF9, 10000)
    problem.UF <- function(X) {
      t(apply(X, MARGIN = 1,
              FUN = UF9
      ))
    }
    }else if (fun == "UF10"){
    UF10 = generateUF(10, in.dim = d, out.dim = n.obj)
    pf = getParetoFront(UF10, 10000)
    problem.UF <- function(X) {
      t(apply(X, MARGIN = 1,
              FUN = UF10
      ))
    }
  }
  out <- list(problem = problem.UF, pf = pf)
  return(out)
}
