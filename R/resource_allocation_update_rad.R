resource_allocation_update_rad <- function(iter,
                                           resource.allocation,
                                           priority.values,
                                           bigZ,
                                           dt.bigZ,
                                           neighbors.T,
                                           Y,
                                           dt.Y,
                                           W,
                                           dt.dm,
                                           X,
                                           dt.X,
                                           newObj,
                                           oldObj,
                                           ...) {
  
  out <- list(priority.values = priority.values,
              dt.dm = dt.dm)
  if (iter > resource.allocation$dt) {
    diversity <-
      online_diversity(
        Y = Y,
        dt.Y = dt.Y,
        W = W,
        dt.dm = dt.dm
      )
    out <- list(priority.values = diversity$p,
                dt.dm = diversity$dm)
  }
  return(out)
}


projection <- function(a, b, epsilon = 1e-50) {
  return(c(sum(a * b) / sum(a ^ 2) + epsilon) * a)
}


find_indexes <- function(Y, dt.Y) {
  # j in eq (3)
  indexes_j <- list() # Y
  # i in eq (3)
  indexes_i <- list() # dt.Y
  for (j in 1:nrow(Y)) {
    found <- FALSE
    min.value <- Inf
    l <- 0
    # equation (4)
    
    for (i in 1:nrow(dt.Y)) {
      set <- rbind(dt.Y[i, ], Y[j, ])
      if (is_maximally_dominated(set)[1]) {
        if (!found) {
          indexes_j <- append(indexes_j, j)
          found <- TRUE
        }
        aux <- norm(dt.Y[i, ] - Y[j, ], type = "2")
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
  function(Y, dt.Y, W, dt.dm, epsilon = 1e-50) {
    out <-
      find_indexes(apply(Y, 2, sample), dt.Y)
    indexes_i <- out$i
    indexes_j <- out$j
    
    # my.out <- rep(.Machine$double.xmin, nrow(Y))
    my.out <- rep(.Machine$double.xmin, nrow(Y))
    
    # equation (7)
    for (i in 1:dim(Y)[1]) {
      # max.out <- -Inf
      if (length(indexes_j) > 0) {
        for (j in 1:length(indexes_j)) {
          # diversity measurement: aumount of diversity loss of an ind. solution between 2 generations
          #condition for eq (7)
          c.line <- Y[i, ] - Y[indexes_j[[j]], ]
          p.line <- dt.Y[i, ] - dt.Y[indexes_i[[j]], ]
          
          if (c.line != 0 && p.line != 0) {
            #equation (3)
            d.convs <-
              (Y[indexes_j[[j]], ] - dt.Y[indexes_i[[j]], ]) + epsilon
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
    # p <-  my.out - dt.dm
    p <-  my.out
    p <- (p - min(p)) / ((max(p) - min(p)) + epsilon)
    # out <- list(p = 1 - p, dm = my.out)
    out <- list(p = p, dm = my.out)
    return(out)
  }

