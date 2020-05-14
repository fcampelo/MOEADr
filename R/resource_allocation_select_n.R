resource_allocation_select_n <-
  function(iter, resource.allocation, W, priority.values, problem, idx.boundary, epsilon = 1e-50, ...) {
    iteration_usage <- rep(TRUE, dim(W)[1])
    indexes <- 1:dim(W)[1]
    if (iter > resource.allocation$dt) {
      iteration_usage <- rep(FALSE, dim(W)[1])
        candidates.idx <- 1:dim(W)[1]
        aux.boundary <- which(candidates.idx %in% idx.boundary)
        candidates.idx <- candidates.idx[-aux.boundary]
        priority.values <- priority.values[-aux.boundary]
        indexes <- sample(x = candidates.idx, resource.allocation$n - problem$m, prob = priority.values + epsilon)
        iteration_usage[indexes] <- TRUE
        iteration_usage[aux.boundary] <- TRUE
    }
    
    
    out <- list(indexes = indexes, iteration_usage = iteration_usage)
    return(out)
  }