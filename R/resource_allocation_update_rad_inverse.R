resource_allocation_update_rad_inverse <- function(iter,
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
    out <- list(priority.values = 1 - diversity$p,
                dt.dm = diversity$dm)
  }
  return(out)
}

