


calculate_DRA <-
  function(resource.allocation,
           neighbors,
           aggfun,
           X,
           W,
           Y,
           preset,
           problem) {
    scaling   <- preset$scaling
    ra <- init_dra(neighbors, aggfun, X, W, Y, scaling)
    oldObj <- ra$oldObj
    idx.boundary <- ra$idx.boundary
    idx.tour <- ra$idx.tour
    size <- floor(dim(W)[1] / 5) - problem$m
    out <-
      list (idx.boundary = idx.boundary,
            idx.tour = idx.tour,
            oldObj = oldObj)
    return (out)
  }

resource_allocation_select_dra <-
  function(iter,
           resource.allocation,
           W,
           priority.values,
           problem,
           ...) {
    indexes <- 1:dim(W)[1]
    iteration_usage <- rep(TRUE, dim(W)[1])
    if (iter > resource.allocation$dt) {
      size <- floor(dim(W)[1] / 5) - problem$m
      idx.tour <-
        selTournament(
          fitness = -priority.values,
          n.select = size,
          k = 10
        )
      indexes <- append(idx.boundary, idx.tour)
      iteration_usage[!indexes] <- 0
    }
    out <-
      list(indexes = indexes, iteration_usage = iteration_usage)
    return(out)
  }