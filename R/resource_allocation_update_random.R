resource_allocation_update_random <- function(iter, 
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
    priority.values <- by_random(dim(W)[1])
  }
  out <- list(priority.values = priority.values)
  return(out)
}

by_random <- function(len) {
  return(runif(len))
}
