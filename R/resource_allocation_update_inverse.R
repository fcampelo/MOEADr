resource_allocation_update_inverse <- function(iter,
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
  if (iter > 2) {
    priority.values <- by_norm(X = X, dt.X = dt.X)
    priority.values <- 1 - priority.values
  }
  out <- list(priority.values = priority.values)
  return(out)
}
