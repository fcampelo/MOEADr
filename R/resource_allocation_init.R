resource_allocation_init <-
  function(resource.allocation, W, ...) {
    # ========== Error catching and default value definitions
    assertthat::assert_that(
      "name" %in% names(resource.allocation),
      is.matrix(W))
    
    # fixed indexes for reproducibility
    indexes <- (1:dim(W)[1])
    priority.values = init_p(W, 1)
    
    # list of previous values from a window (DELTA T = 20) for GRA
    dt.bigZ   = list()
    
    # null hacks from DRA for reproducibility
    oldObj <- NULL
    idx.boundary <- NULL
    idx.tour <- NULL
    if (resource.allocation$name == "dra") {
      dra_hacks <- init_dra(neighbors, aggfun, X, W, Y, scaling)
      oldObj <- dra_hacks$oldObj
      idx.boundary <- dra_hacks$idx.boundary
      idx.tour <- dra_hacks$idx.tour
    }
    
    out <- list(
      indexes    = indexes,
      dt.bigZ   = dt.bigZ,
      priority.values = priority.values,
      oldOb = oldObj,
      idx.boundary = idx.boundary)
    return(out)
  }


### ugly dra init code
init_dra <- function(neighbors, aggfun, X, W, Y, scaling) {
  priority.values <- init_p(W, 1)
  
  BP <- define_neighborhood(neighbors = neighbors,
                            v.matrix  = switch(neighbors$name,
                                               lambda = W,
                                               x      = X),
                            iter      = 1)
  my.identity <- diag(dim(W)[2])
  idx <- list()
  for (i in 1:dim(W)[1]) {
    for (j in 1:dim(my.identity)[1]) {
      my.sum <- sum(round(W[i,], 2) == my.identity[j,])
      if (my.sum == dim(W)[2]) {
        idx[[length(idx) + 1]] <- i
      }
    }
  }
  idx.boundary <- unlist(idx)
  # ========== Scalarization
  # Objective scaling and estimation of 'ideal' and 'nadir' points
  Yt <- matrix(0, dim(W)[1], dim(W)[2])
  normYs <- scale_objectives(Y       = Y,
                             Yt      = Yt,
                             scaling = scaling)
  
  # Scalarization by neighborhood.
  B  <- BP$B.scalarize
  bigZ <- scalarize_values(
    normYs  = normYs,
    W       = W,
    B       = B,
    aggfun  = aggfun
  )
  oldObj <- bigZ[neighbors$T + 1,]
  return(
    list (
      priority.values      = priority.values,
      oldObj = oldObj,
      idx.boundary = idx.boundary
    )
  )
}