resource_allocation_update_ri <- function(iter, 
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
  if (iter > resource.allocation$dt) {
    priority.values <-
      by_RI(dt.bigZ, bigZ, neighbors.T, epsilon = 1e-50)
  }
  out <- list(priority.values = priority.values)
  return(out)
}



by_RI <- function(dt.bigZ, bigZ, neighbors.T, epsilon = 1e-50) {
  # dt <- apply(X = dt.bigZ, MARGIN = 2, sum)
  dt <- dt.bigZ[neighbors.T + 1,]
  z <- bigZ[neighbors.T + 1,]
  u <- (dt - z) / dt
  if (max(u) == 0) {
    p <- rep(1, length(u))
  }
  else{
    p <- (u + epsilon) / (max(u) + epsilon)
  }
  return(p)
}