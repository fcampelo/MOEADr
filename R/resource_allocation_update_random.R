#' Resource Allocation random
#'
#' Proxy Resource Allocation random - random
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
#' @return priority values for each subproblem based on random values `out`'.
#'
#' @section References:
#'
#' F. Campelo, L.S. Batista, C. Aranha (2020): The {MOEADr} Package: A
#' Component-Based Framework for Multiobjective Evolutionary Algorithms Based on
#' Decomposition. Journal of Statistical Software \doi{10.18637/jss.v092.i06}\cr
#'
#' @export
resource_allocation_update_random <- function(iter,
                                              resource.allocation,
                                              priority.values,
                                              bigZ,
                                              dt.bigZ,
                                              neighbors.T,
                                              Y,
                                              dt.Y,
                                              W,
                                              X,
                                              dt.X,
                                              # newObj,
                                              # oldObj,
                                              ...) {
  if (iter > resource.allocation$dt) {
    priority.values <- by_random(dim(W)[1])
  }
  out <- list(priority.values = priority.values)
  return(out)
}

by_random <- function(len) {
  return(runif(len))
}
