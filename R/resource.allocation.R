ONRA <- function(dt.bigZ, bigZ, my.T, epsilon = 1e-50) {
  # dt <- apply(X = dt.bigZ, MARGIN = 2, sum)
  dt <- dt.bigZ[my.T + 1,]
  z <- bigZ[my.T + 1,]
  u <- (dt - z) / dt
  if (max(u) == 0) {
    p <- rep(1, length(u))
  }
  else{
    p <- (u + epsilon) / (max(u) + epsilon)
  }
  return(p)
}

# L2
norm_vec2 <- function(x) {
  sqrt(crossprod(x))
}


projection <- function(a, b, epsilon = 1e-50) {
  return(c(sum(a * b) / sum(a ^ 2) + epsilon) * a)
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
      set <- rbind(parent[i,], offspring[j,])
      if (is_maximally_dominated(set)[1]) {
        if (!found) {
          indexes_j <- append(indexes_j, j)
          found <- TRUE
        }
        aux <- norm(parent[i,] - offspring[j,], type = "2")
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
    out <-
      find_indexes(apply(offspring, 2, sample), parent)
    indexes_i <- out$i
    indexes_j <- out$j
    # my.out <- rep(.Machine$double.xmin, nrow(offspring))
    my.out <- rep(.Machine$double.xmin, nrow(offspring))
    
    # equation (7)
    for (i in 1:dim(offspring)[1]) {
      # max.out <- -Inf
      if (length(indexes_j) > 0) {
        for (j in 1:length(indexes_j)) {
          # diversity measurement: aumount of diversity loss of an ind. solution between 2 generations
          #condition for eq (7)
          c.line <- offspring[i,] - offspring[indexes_j[[j]],]
          p.line <- parent[i,] - parent[indexes_i[[j]],]
          
          if (c.line != 0 && p.line != 0) {
            #equation (3)
            d.convs <-
              (offspring[indexes_j[[j]],] - parent[indexes_i[[j]],]) + epsilon
            # projection calculation
            proj.c <- projection(c.line, d.convs)
            proj.p <- projection(p.line, d.convs)
            
            # calculate the norm of the vectors: numerator and demoniator of equation (7)
            a <- norm(p.line - proj.p, type = "2")
            b <- norm(c.line - proj.c, type = "2")
            
            # equation (7)
            aux <-  a / (b + epsilon)
            #equation (10)
            if (aux > my.out[i]) {
              my.out[i] <- aux
            }
          }
          
        }
      }
      else{
        my.out[i] <- .Machine$double.xmax
      }
    }
    # p <-  my.out - old.dm
    p <-  my.out
    p <- (p - min(p)) / ((max(p) - min(p)) + epsilon)
    # out <- list(p = 1 - p, dm = my.out)
    out <- list(p = 1-p, dm = my.out)
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

init_dra <- function(neighbors, aggfun, X, W, Y, scaling) {
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
      my.sum <- sum(round(W[i,], 2) == my.identity[j,])
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
  oldObj <- bigZ[neighbors$T + 1,]
  return(list (
    Pi      = Pi,
    oldObj = oldObj,
    idx.bounday = idx.bounday#,
    # BP = BP
  ))
}

init_gra <- function(neighbors, aggfun, X, W, Y) {
  # for GRA - do not this hardcoded
  Pi <- init_p(W, 1)
  
  return(list (Pi      = Pi))
}

init_rad <- function(neighbors, aggfun, X, W, Y) {
  # for GRA - do not this hardcoded
  Pi <- init_p(W, 1)
  
  return(list (Pi      = Pi))
}



ws_transformation <- function(W, epsilon = 1e-50) {
  temp <- t(apply(W, 1, function(W) {
    1 / (W + epsilon)
  }))
  temp <- t((apply(temp, 1, function(temp) {
    temp / sum(temp)
  })))
  return (round(temp, 8))
}



by_norm <- function(parent_x, offspring_x, epsilon = 1e-50) {
  u <- apply(offspring_x - parent_x, 1, norm, type = "2")
  u <- (u - min(u)) / ((max(u) - min(u)) + epsilon)
  return (u)
}

by_random <- function(len) {
  return(runif(len))
}

by_jacobian <- function(problem, offspring, epsilon = 1e-50) {
  u <- apply(jacobian(problem, offspring), 2, sum)
  u <- (u - min(u)) / ((max(u) - min(u)) + epsilon)
  return (u)
}