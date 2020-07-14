#' Resource Allocation select n
#'
#' Resource Allocation selecting procedure of n subproblems
#'
#' Returns a vector with indexes of each subproblem that will receive resource
#'
#' @param iter integer iteration
#' @param resource.allocation list resource allocation method and specific vars
#' @param W matrix of weights
#' @param priority.values priority values for deciding which subproblem to give resources
#' @param idx.boundary indexes of the boundaries subproblem
#' @param problem list with number of objectives n
#'
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @return indexes and counter of use for each subproblem `out`'.
#'
#' @section References:
#'
#' F. Campelo, L.S. Batista, C. Aranha (2020): The {MOEADr} Package: A
#' Component-Based Framework for Multiobjective Evolutionary Algorithms Based on
#' Decomposition. Journal of Statistical Software \doi{10.18637/jss.v092.i06}\cr
#'
#' @export
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
      indexes <- sample(x = candidates.idx, size = resource.allocation$n - problem$m, prob = priority.values + epsilon)
      # indexes <- sample(x = candidates.idx, resource.allocation$n, prob = priority.values + epsilon)
      iteration_usage[indexes] <- TRUE
      iteration_usage[aux.boundary] <- TRUE
    }


    out <- list(indexes = indexes, iteration_usage = iteration_usage)
    return(out)
  }
