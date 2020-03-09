resource_allocation_update_norm <- function(iter,
                                            resource.allocation,
                                            priority.values,
                                            bigZ,
                                            dt.bigZ,
                                            neighbors.T,
                                            Y,
                                            dt.Y,
                                            W,
                                            X,
                                            dt.X,
                                            # newObj,
                                            # oldObj,
                                            ...) {
  if (iter > resource.allocation$dt) {
    priority.values <- by_norm(X = X, dt.X = dt.X, resource.allocation)
  }
  out <- list(priority.values = priority.values)
  return(out)
}


by_norm <- function(X, dt.X, resource.allocation, epsilon = 1e-50) {
  u <- apply(X - dt.X, 1, norm, type = "2")
  u <- (u - min(u)) / ((max(u) - min(u)) + epsilon)
  return (u)
}