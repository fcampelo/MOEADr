resource_allocation_select_random <-
  function(iter, resource.allocation, W, priority.values, problem, ...) {
    priority.values <- priority.values
    indexes <- 1:dim(W)[1]
    iteration_usage <- rep(TRUE, dim(W)[1])
    if (iter > resource.allocation$dt) {
      rand.seq <- runif(length(priority.values))
      indexes <- which(rand.seq <= priority.values)
      iteration_usage <- (rand.seq <= priority.values)
      # if (length(indexes) < 3 || is.null(length(indexes))) {
      #   indexes <- which(rand.seq <= 1)
      #   iteration_usage <- rep(1, dim(W)[1])
      # }
    }
    out <- list(indexes = indexes, iteration_usage = iteration_usage)
    return(out)
  }