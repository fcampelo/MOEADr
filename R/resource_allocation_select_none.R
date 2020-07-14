#' Resource Allocation dummy, nothing happens here
#'
#' Resource Allocation None - dummy

#'
#' @param resource.allocation which Resource Allocation method
#' @param W matrix of weight
#' @param bigZ bigZ legado
#'
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @return list of indexes and use counter for subproblems `out`'.
#'
#' @section References:
#'
#' F. Campelo, L.S. Batista, C. Aranha (2020): The {MOEADr} Package: A
#' Component-Based Framework for Multiobjective Evolutionary Algorithms Based on
#' Decomposition. Journal of Statistical Software \doi{10.18637/jss.v092.i06}\cr
#'
#' @export

resource_allocation_select_none <-
  function(iter, resource.allocation, W, priority.values, problem, ...) {
    indexes <- 1:dim(W)[1]
    iteration_usage <- rep(TRUE, dim(W)[1])
    out <- list(indexes = indexes, iteration_usage = iteration_usage)
    return(out)
  }
