resource_allocation_select_tour <-
  function(iter, resource.allocation, W, priority.values, problem, ...) {
    indexes <- 1:dim(W)[1]
    iteration_usage <- rep(TRUE, dim(W)[1])
    if (iter > resource.allocation$dt) {
      found <- 0
      size <- ceiling(dim(W)[1] * resource.allocation$size)
      k <- ceiling(dim(W)[1] * resource.allocation$k)
      indexes <- vector(length = size)
      temp.idx <- !vector(length = size)
      
      # without repeted subproblems
      while (TRUE) {
        indexes[temp.idx] <-
          selTournament(
            fitness = priority.values,
            n.select = size - found,
            k = k
          )
        if (length(unique(indexes)) == length(indexes)) {
          break
        } else {
          temp.idx <- !unique(indexes)
          indexes <- unique(indexes)
          found <- found + length(indexes)
        }
      }
      iteration_usage[-indexes] <- FALSE
    }
    out <-
      list(indexes = indexes, iteration_usage = iteration_usage)
    return(out)
  }