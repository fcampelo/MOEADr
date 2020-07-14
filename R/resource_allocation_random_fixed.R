resource_allocation_update_random_fixed <- function(iter, 
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
    priority.values <- by_random_fixed(dim(W)[1], resource.allocation)
  }
  out <- list(priority.values = priority.values)
  return(out)
}

by_random_fixed <- function(len, resource.allocation) {
  return(rep(resource.allocation$fixed_value, len))
}
