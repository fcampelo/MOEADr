ONRA <- function(dt.bigZ, bigZ, my.T, epsilon = 1e-50) {
  # dt <- apply(X = dt.bigZ, MARGIN = 2, sum)
  dt <- dt.bigZ[my.T + 1, ]
  z <- bigZ[my.T + 1, ]
  u <- (dt - z) / dt
  if (max(u) == 0) {
    p <- rep(1, length(u))
  }
  else{
    p <- (u + epsilon) / (max(u) + epsilon)
  }
  return(p)
}


norm_vec2 <- function(x) {
  sqrt(crossprod(x))
}

norm_vec2.2 <- function(x) {
  crossprod(x)
}


projection <- function(a, b, epsilon = 1e-50) {
  return(c(sum(a * b) / (norm_vec2.2(a) + epsilon)) * a)
}


find_indexes <- function(offspring, parent) {
  # j in eq (3)
  indexes_j <- list() # offspring
  # i in eq (3)
  indexes_i <- list() # parent
  for (j in 1:nrow(offspring)) {
    found <- FALSE
    min.value <- Inf
    l <- 0
    # equation (4)
    
    for (i in 1:nrow(parent)) {
      set <- cbind(parent[i, ], offspring[j, ])
      if (is_dominated(set)[1]) {
        if (!found) {
          indexes_j <- append(indexes_j, j)
          found <- TRUE
        }
        aux <- norm_vec2(parent[i, ] - offspring[j, ])
        if (min.value > aux) {
          # for equation (6) and (5)
          min.value <- aux
          l <- i
        }
      }
    }
    
    if (l != 0)
      # i in equation (5)
      indexes_i <- append(indexes_j, l)
  }
  out <- list(i = indexes_i, j = indexes_j)
  return(out)
}



online_diversity <-
  function(offspring, parent, W, old.dm, epsilon = 1e-50) {
    out <- find_indexes(offspring, parent)
    indexes_i <- out$i
    indexes_j <- out$j
    
    # "onra"
    # p <- rep(0.5, nrow(offspring))
    my.out <- rep(-Inf, nrow(offspring))
    
    # equation (7)
    for (i in 1:dim(offspring)[1]) {
      # max.out <- -Inf
      if (length(indexes_j) > 0) {
        for (j in 1:length(indexes_j)) {
          # diversity measurement: aumount of diversity loss of an ind. solution between 2 generations
          #condition for eq (7)
          c.line <- offspring[i, ] - offspring[indexes_j[[j]], ]
          p.line <- parent[i, ] - parent[indexes_i[[j]], ]
          
          #equation (3)
          d.convs <-
            (offspring[indexes_j[[j]], ] - parent[indexes_i[[j]], ]) + epsilon
          
          # projection calculation
          proj.c <- projection(c.line, d.convs)
          proj.p <- projection(p.line, d.convs)
          
          # calculate the norm of the vectors: numerator and demoniator of equation (7)
          a <- norm_vec2(p.line - proj.p)
          b <- norm_vec2(c.line - proj.c)
          
          # equation (7)
          aux <-  a / (b + epsilon)
          #equation (10)
          if (aux > my.out[i]) {
            my.out[i] <- aux
          }
        }
      }
      else{
        my.out[i] <- Inf
      }
    }
    p <-  my.out - old.dm
    p <- (p - min(p)) / ((max(p) - min(p)) + epsilon)
    if (anyNA(p))
      p <- init_p(W, 1)
    out <- list(p = p, dm = my.out)
    return(out)
  }



dra <- function(newObj, oldObj, Pi) {
  # calcuate utility function
  delta <- (oldObj - newObj) / oldObj
  temp <- delta <= 0.001
  
  Pi.not.idx <- which(temp == FALSE)
  Pi[Pi.not.idx] = 1
  
  Pi.idx <- which(temp == TRUE)
  Pi[Pi.idx] = (0.95 + (0.05 * (delta[Pi.idx] / 0.001))) * Pi[Pi.idx]
  
  return(Pi)
}

init_dra <- function(neighbors, aggfun, X, W, Y) {
  BP <- define_neighborhood(neighbors = neighbors,
                            v.matrix  = switch(neighbors$name,
                                               lambda = W,
                                               x      = X),
                            iter      = 1)
  # for DRA - do not this hardcoded
  Pi <- init_p(W, 1)
  
  my.identity <- diag(dim(W)[2])
  idx <- list()
  for (i in 1:dim(W)[1]) {
    for (j in 1:dim(my.identity)[1]) {
      my.sum <- sum(round(W[i, ],2) == my.identity[j, ])
      if (my.sum == dim(W)[2]) {
        idx[[length(idx) + 1]] <- i
      }
    }
  }
  idx.bounday <- unlist(idx)
  # ========== Scalarization
  # Objective scaling and estimation of 'ideal' and 'nadir' points
  Yt <- matrix(0, dim(W)[1], dim(W)[2])
  normYs <- scale_objectives(Y       = Y,
                             Yt      = Yt,
                             scaling = scaling)
  
  # Scalarization by neighborhood.
  # bigZ is an [(T+1) x N] matrix, in which each column has the T scalarized
  # values for the solutions in the neighborhood of one subproblem, plus the
  # scalarized value for the incumbent solution for that subproblem.
  B  <- BP$B.scalarize
  bigZ <- scalarize_values(
    normYs  = normYs,
    W       = W,
    B       = B,
    aggfun  = aggfun
  )
  oldObj <- bigZ[neighbors$T + 1, ]
  return(list (
    Pi      = Pi,
    oldObj = oldObj,
    idx.bounday = idx.bounday,
    BP = BP
  ))
}

init_gra <- function(neighbors, aggfun, X, W, Y) {
  BP <- define_neighborhood(neighbors = neighbors,
                            v.matrix  = switch(neighbors$name,
                                               lambda = W,
                                               x      = X),
                            iter      = 1)
  # for GRA - do not this hardcoded
  Pi <- init_p(W, 1)
  
  return(list (Pi      = Pi,
               BP = BP))
}

init_rad <- function(neighbors, aggfun, X, W, Y) {
  BP <- define_neighborhood(neighbors = neighbors,
                            v.matrix  = switch(neighbors$name,
                                               lambda = W,
                                               x      = X),
                            iter      = 1)
  # for GRA - do not this hardcoded
  Pi <- init_p(W, 0.5)
  
  return(list (Pi      = Pi,
               BP = BP))
}