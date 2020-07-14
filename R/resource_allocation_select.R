#' Resource Allocation select
#'
#' Proxy for Resource Allocation selecting procedure
#'
#' Returns a vector with indexes of each subproblem that will receive resource
#'
#' @param iter integer iteration
#' @param resource.allocation list resource allocation method and specific vars
#' @param W matrix of weights
#' @param priority.values priority values for deciding which subproblem to give resources
#' @param idx.boundary indexes of the boundaries subproblem
#'
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @return indexes and counter of subproblem usage `out`'.
#'
#' @section References:
#'
#' F. Campelo, L.S. Batista, C. Aranha (2020): The {MOEADr} Package: A
#' Component-Based Framework for Multiobjective Evolutionary Algorithms Based on
#' Decomposition. Journal of Statistical Software \doi{10.18637/jss.v092.i06}\cr
#'
#' @export
resource_allocation_select <-
  function(iter,
           resource.allocation,
           W,
           priority.values,
           problem,
           idx.boundary = NULL,
           ...) {
    # ========== Error catching and default value definitions
    assertthat::assert_that(
      is.numeric(iter),
      "selection" %in% names(resource.allocation),
      length(priority.values) == dim(W)[1],
      is.matrix(W))

    # ========== Call specific resource allocation strategy
    # to add the bounderies
    function_name <- paste0("resource_allocation_select_", tolower(resource.allocation$selection))
    updt.args     <- as.list(sys.call())[-1]

    selected       <- do.call(function_name,
                             args = updt.args)
    indexes <- selected$indexes
    iteration_usage <- selected$iteration_usage

    # to add the bounderies
    if(length(indexes) != length(priority.values)){
      for (i in 1:length(idx.boundary)){
        indexes[length(indexes)+i] <- idx.boundary[i]
      }
    }

    indexes <- sort(indexes)


    out <-
      list(
        indexes = indexes,
        iteration_usage = iteration_usage
      )

    return(out)
  }



