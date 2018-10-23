norm_vec2 <- function(x) {
  sqrt(crossprod(x))
}

projection <- function(a, b, epsilon = 1e-50) {
  return(c(sum(a * b) / (norm_vec2(a) ^ 2 + epsilon)) * a)
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
      if (emoa::is_dominated(set)[1] || emoa::is_maximally_dominated(set)[1]) {
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
  function(offspring, parent, W, epsilon = 1e-50) {
    out <- find_indexes(offspring, parent)
    indexes_i <- out$i
    indexes_j <- out$j
    
    # "onra"
    # p <- rep(0.5, nrow(offspring))
    my.out <- rep(-Inf, nrow(offspring))
    
    # equation (7)
    for (i in 1:dim(offspring)[1]) {
      # max.out <- -Inf
      for (j in 1:length(indexes_j)) {
        # diversity measurement: aumount of diversity loss of an ind. solution between 2 generations
        #condition for eq (7)
        c.line <- offspring[i,] - offspring[indexes_j[[j]],]
        p.line <- parent[i,] - parent[indexes_i[[j]],]
        
        #equation (3)
        d.convs <-
          (offspring[indexes_j[[j]],] - parent[indexes_i[[j]],]) + epsilon
        
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
    
    # create vector p for "ONRA"
      p <- my.out
      # print(length(p))
    # }
    # scalarize p between 0 and 1
    p <- (p - min(p)) / (max(p) - min(p))
    out <- list(p = p, dm = my.out)
    return(out)
  }
