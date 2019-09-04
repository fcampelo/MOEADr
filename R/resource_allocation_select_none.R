resource_allocation_select_none <-
  function(iter, resource.allocation, W, priority.values, problem, ...) {
    indexes <- 1:dim(W)[1]
    iteration_usage <- rep(TRUE, dim(W)[1])
    out <- list(indexes = indexes, iteration_usage = iteration_usage)
    return(out)
  }