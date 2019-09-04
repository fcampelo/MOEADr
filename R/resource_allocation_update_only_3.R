resource_allocation_update_only_3 <- function(iter,
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
    priority.values <- by_3(W)
  }
  out <- list(priority.values = priority.values)
  return(out)
}


by_3 <- function(W, epsilon = 1e-50) {
  u <- sample(1:dim(W)[1], size = 1)
  priority.values <- rep(0, dim(W)[1])
  priority.values[u] <- 1
  priority.values[1] <- 1
  priority.values[dim(W)[1]] <- 1
  return (priority.values)
}