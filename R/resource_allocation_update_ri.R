#' Resource Allocation RI
#'
#' Proxy Resource Allocation RI - Relative imprvoement 
#'
#' Returns a vector with indexes of each subproblem that will receive resource
#'
#' @param iter integer iteration
#' @param resource.allocation list resource allocation method and specific vars
#' @param priority.values priority values for deciding which subproblem to give resources
#' @param idx.boundary indexes of the boundaries subproblem
#' @param problem list with number of objectives n
#' @param bigZ agg values for RI
#' @param dt.bigZ agg values for RI from dt previous iteration
#' @param neighbors.T neighborhood size (unused?)
#' @param Y PF
#' @param dt.Y PF from dt previous iteration
#' @param X decision values
#' @param dt.X decision values from dt previous iteration
#'
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @return priority values for each subproblem based on convergence `out`'.
#'
#' @section References:
#'
#' F. Campelo, L.S. Batista, C. Aranha (2020): The {MOEADr} Package: A
#' Component-Based Framework for Multiobjective Evolutionary Algorithms Based on
#' Decomposition. Journal of Statistical Software \doi{10.18637/jss.v092.i06}\cr
#'
#' @export
resource_allocation_update_ri <- function(iter,
                                          resource.allocation,
                                          priority.values,
                                          bigZ,
                                          dt.bigZ,
                                          neighbors.T,
                                          Y,
                                          dt.Y,
                                          W,
                                          # dt.dm,
                                          X,
                                          dt.X,
                                          # newObj,
                                          # oldObj,
                                          ...) {
  if (iter > resource.allocation$dt) {
    priority.values <-
      by_RI(dt.bigZ, bigZ, neighbors.T, resource.allocation, epsilon = 1e-50)
  }
  out <- list(priority.values = priority.values)
  return(out)
}



by_RI <- function(dt.bigZ, bigZ, neighbors.T, resource.allocation, epsilon = 1e-50) {
  # dt <- apply(X = dt.bigZ, MARGIN = 2, sum)
  dt <- dt.bigZ[neighbors.T + 1,]
  
  z <- bigZ[neighbors.T + 1,]
  u <- (dt - z) / dt
  if (max(u) == 0) {
    u <- rep(1, length(u))
  }
  else{
    # p <- (u + epsilon) / (max(u) + epsilon)
    u <- (u - min(u)) / ((max(u) - min(u)) + epsilon)
  }
  # print("p")
  # print(p)
  # p<-replace(p, p<0, 0)
  # print(p)
  return(u)
}
