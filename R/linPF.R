library(smoof)

makeLINPF1Function <- function(dimensions = 12L, n.objectives = 3L) {
  fn <- function(X) {
    # assertNumeric(x, any.missing = FALSE, all.missing = FALSE, finite = TRUE)
    X <- t(X)
    
    f <- matrix(rep(0, n.objectives), nrow = n.objectives)
    
    x <- matrix(c(X[1], X[2:(n.objectives - 1)] * pi * 2), ncol = 2)
    
    g <-
      (1 + sin(pi * x[1] / 2)) * sum((X[n.objectives:dim(X)[2]] - sin(pi * x[1] /
                                                                         2)) ^ 2)
    f[1, ] <- 1 - X[1] * cos(X[2])
    for (i in 2:(n.objectives - 1)) {
      if (i == 2) {
        f[i, ] <- 2 + x[1] * (cos(x[2])) + sin(x[i])
      }
      else{
        f[i, ] <-
          2 + x[1] * (cos(x[2]) - sum(sin(x[2:(i - 1)]))    + sin(x[i]))
      }
      
    }
    f[n.objectives, ] <- n.objectives + x[1] * (cos(x[2]) - sin(x[ 2:(n.objectives - 1)]))
    
    centre = rep(1, n.objectives) / n.objectives
    Y <- matrix(rep(1 + g, n.objectives), ncol = 3) * t(centre + f)
    
    colnames(Y) <- paste0("f", 1:n.objectives)
    
    return (Y)
  }
  
  makeMultiObjectiveFunction(
    name = "linPF1 Function",
    id = "LINPF1",
    description = "Shouyoung et. al",
    fn = fn,
    par.set = makeNumericParamSet(
      len = 12L,
      id = "x",
      lower = rep(0, 12L),
      upper = rep(1, 12L),
      vector = T
    ),
    n.objectives = n.objectives
  )
}

makeLINPF2Function <- function(dimensions = 12L, n.objectives = 3L) {
  fn <- function(X) {
    X <- t(X)
    
    f <- matrix(rep(0, n.objectives), nrow = n.objectives)
    
    x <- matrix(c(0.2 * X[1] + 0.8, X[2:(n.objectives - 1)] * pi * 2), ncol = 2)
    
    g <-
      (1 + sin(pi * X[1] / 2)) * sum((X[n.objectives:dim(X)[2]] - sin(pi * X[1] /
                                                                         2)) ^ 2)
    
    
    
    f[1, ] <- 1 - X[1] * cos(X[2])
    for (i in 2:(n.objectives - 1)) {
      if (i == 2) {
        f[i, ] <- 2 + X[1] * (cos(X[2]) - rep(0, dim(x)[1]) + sin(X[i]))
      }
      else{
        f[i, ] <-
          2 + X[1] * (cos(X[2]) - sum(sin(X[2:(i - 1)]))    + sin(X[i]))
      }
      
    }
    f[n.objectives, ] <- n.objectives + X[1] * (cos(X[2]) - sin(X[2:(n.objectives - 1)]))
    
    centre = rep(1, n.objectives) / n.objectives
    Y <- matrix(rep(1 + g, n.objectives), ncol = 3) * t(centre + f)
    
    
    colnames(Y) <- paste0("f", 1:n.objectives)
    
    return (Y)
  }
  
  makeMultiObjectiveFunction(
    name = "linPF2 Function",
    id = "LINPF2",
    description = "Shouyoung et. al",
    fn = fn,
    par.set = makeNumericParamSet(
      len = 12L,
      id = "x",
      lower = rep(0, 12L),
      upper = rep(1, 12L),
      vector = T
    ),
    n.objectives = n.objectives
  )
}


makeLINPF3Function <- function(dimensions = 12L, n.objectives = 3L) {
  fn <- function(X) {
    X <- t(X)
    
    f <- matrix(rep(0, n.objectives), nrow = n.objectives)
    
    x <-
      matrix(c(0.5 * X[1] + 0.5, X[2:(n.objectives - 1)] * pi / 2 + 2 * pi), ncol = 2)
    
    g <-
      (1 + sin(pi * X[1] / 2)) * sum((X[n.objectives:dim(X)[2]] - sin(pi * X[1] /
                                                                         2)) ^ 2)
    
    
    
    f[1, ] <- 1 - X[1] * cos(X[2])
    for (i in 2:(n.objectives - 1)) {
      if (i == 2) {
        f[i, ] <- 2 + X[1] * (cos(X[2]) - rep(0, dim(x)[1]) + sin(X[i]))
      }
      else{
        f[i, ] <-
          2 + X[1] * (cos(X[2]) - sum(sin(X[2:(i - 1)]))    + sin(X[i]))
      }
      
    }
    f[n.objectives, ] <- n.objectives + X[1] * (cos(X[2]) - sin(X[2:(n.objectives - 1)]))
    
    centre = rep(1, n.objectives) / n.objectives
    Y <- matrix(rep(1 + g, n.objectives), ncol = 3) * t(centre + f)
    
    
    colnames(Y) <- paste0("f", 1:n.objectives)
    
    return (Y)
  }
  
  makeMultiObjectiveFunction(
    name = "linPF3 Function",
    id = "LINPF3",
    description = "Shouyoung et. al",
    fn = fn,
    par.set = makeNumericParamSet(
      len = 12L,
      id = "x",
      lower = rep(0, 12L),
      upper = rep(1, 12L),
      vector = T
    ),
    n.objectives = n.objectives
  )
}

makeLINPF4Function <- function(dimensions = 12L, n.objectives = 3L) {
  fn <- function(X) {
    X <- t(X)
    
    f <- matrix(rep(0, n.objectives), nrow = n.objectives)
    
    x <-
      matrix(c(0.2 * X[1] + 0.8, X[2:(n.objectives - 1)] * pi / 2 + pi / 4), ncol = 2)
    
    g <-
      (1 + sin(pi * X[1] / 2)) * sum((X[n.objectives:dim(X)[2]] - sin(pi * X[1] /
                                                                         2)) ^ 2)
    
    
    
    f[1, ] <- 1 - X[1] * cos(X[2])
    for (i in 2:(n.objectives - 1)) {
      if (i == 2) {
        f[i, ] <- 2 + X[1] * (cos(X[2]) - rep(0, dim(x)[1]) + sin(X[i]))
      }
      else{
        f[i, ] <-
          2 + X[1] * (cos(X[2]) - sum(sin(X[2:(i - 1)]))    + sin(X[i]))
      }
      
    }
    f[n.objectives, ] <- n.objectives + X[1] * (cos(X[2]) - sin(X[2:(n.objectives - 1)]))
    
    centre = rep(1, n.objectives) / n.objectives
    Y <- matrix(rep(1 + g, n.objectives), ncol = 3) * t(centre + f)
    
    
    colnames(Y) <- paste0("f", 1:n.objectives)
    
    return (Y)
  }
  
  makeMultiObjectiveFunction(
    name = "linPF4 Function",
    id = "LINPF4",
    description = "Shouyoung et. al",
    fn = fn,
    par.set = makeNumericParamSet(
      len = 12L,
      id = "x",
      lower = rep(0, 12L),
      upper = rep(1, 12L),
      vector = T
    ),
    n.objectives = n.objectives
  )
}

makeLINPF5Function <- function(dimensions = 12L, n.objectives = 3L) {
  fn <- function(X) {
    X <- t(X)
    
    f <- matrix(rep(0, n.objectives), nrow = n.objectives)
    
    x <- matrix(c(X[1], X[2:(n.objectives - 1)] * pi * 2), ncol = 2)
    
    g <- sum((X[n.objectives:dim(X)[2]] - 0.5) ^ 2) + abs(sin(pi / 2 * round(2 *X[1])))
  
    
    
    f[1, ] <- 1 - X[1] * cos(X[2])
    for (i in 2:(n.objectives - 1)) {
      if (i == 2) {
        f[i, ] <- 2 + X[1] * (cos(X[2]) - rep(0, dim(x)[1]) + sin(X[i]))
      }
      else{
        f[i, ] <-
          2 + X[1] * (cos(X[2]) - sum(sin(X[2:(i - 1)]))    + sin(X[i]))
      }
      
    }
    f[n.objectives, ] <- n.objectives + X[1] * (cos(X[2]) - sin(X[2:(n.objectives - 1)]))
    
    centre = rep(1, n.objectives) / n.objectives
    Y <- matrix(rep(1 + g, n.objectives), ncol = 3) * t(centre + f)
    
    
    colnames(Y) <- paste0("f", 1:n.objectives)
    
    return (Y)
  }
  
  makeMultiObjectiveFunction(
    name = "linPF5 Function",
    id = "linPF5",
    description = "Shouyoung et. al",
    fn = fn,
    par.set = makeNumericParamSet(
      len = dimensions,
      id = "x",
      lower = rep(0, dimensions),
      upper = rep(1, dimensions),
      vector = T
    ),
    n.objectives = n.objectives
  )
}

makeLINPF6Function <- function(dimensions = 12L, n.objectives = 3L) {
  fn <- function(X) {
    X <- t(X)
    
    f <- matrix(rep(0, n.objectives), nrow = n.objectives)
    
    x <- matrix(c(X[1],X[2:(n.objectives - 1)]*2*pi), ncol = 2)
    
    g <- (1+sin(pi*X[1]/2))*sum((X[n.objectives:dim(X)[2]]-sin(pi*X[1]/2))^2)+sin(pi/2*floor(2*X[1]+1e-5))
    
    f[1, ] <- 1 - X[1] * cos(X[2])
    for (i in 2:(n.objectives - 1)) {
      if (i == 2) {
        f[i, ] <- 2 + X[1] * (cos(X[2]) - rep(0, dim(x)[1]) + sin(X[i]))
      }
      else{
        f[i, ] <-
          2 + X[1] * (cos(X[2]) - sum(sin(X[2:(i - 1)]))    + sin(X[i]))
      }
      
    }
    f[n.objectives, ] <- n.objectives + X[1] * (cos(X[2]) - sin(X[2:(n.objectives - 1)]))
    
    centre = rep(1, n.objectives) / n.objectives
    Y <- matrix(rep(1 + g, n.objectives), ncol = 3) * t(centre + f)
    
    
    colnames(Y) <- paste0("f", 1:n.objectives)
    
    return (Y)
  }
  
  makeMultiObjectiveFunction(
    name = "linPF6 Function",
    id = "LINPF6",
    description = "Shouyoung et. al",
    fn = fn,
    par.set = makeNumericParamSet(
      len = 12L,
      id = "x",
      lower = rep(0, 12L),
      upper = rep(1, 12L),
      vector = T
    ),
    n.objectives = n.objectives
  )
}


# class(makeLINPF1Function) = c("function", "smoof_generator")
# attr(makeLINPF1Function, "name") = c("LINPF1")
# attr(makeLINPF1Function, "type") = c("multi-objective")
# attr(makeLINPF1Function, "tags") = c("multi-objective")