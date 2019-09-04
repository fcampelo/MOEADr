resource_allocation_update_dra <- function(iter, 
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
  if (iter %% resource.allocation$dt == 0) {
    newObj <- bigZ[neighbors$T + 1,]
    priority.values <- dra(newObj, oldObj, priority.values)
  }
  out <- list(priority.values = priority.values)
  return(out)
}


dra <- function(newObj, oldObj, priority.values) {
  # calcuate utility function
  delta <- (oldObj - newObj) / oldObj
  temp <- delta <= 0.001
  
  priority.values.not.idx <- which(temp == FALSE)
  priority.values[priority.values.not.idx] = 1
  
  priority.values.idx <- which(temp == TRUE)
  priority.values[priority.values.idx] = (0.95 + (0.05 * (delta[priority.values.idx] / 0.001))) * priority.values[priority.values.idx]
  
  return(priority.values)
}