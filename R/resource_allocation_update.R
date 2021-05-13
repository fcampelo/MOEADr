#' Resource Allocation update
#'
#' Proxy Resource Allocation update
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
#' @return priority values for each subproblem `out`'.
#'
#' @section References:
#'
#' F. Campelo, L.S. Batista, C. Aranha (2020): The {MOEADr} Package: A
#' Component-Based Framework for Multiobjective Evolutionary Algorithms Based on
#' Decomposition. Journal of Statistical Software \doi{10.18637/jss.v092.i06}\cr
#'
#' @export
resource_allocation_update <-
  function(iter,
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
           ...) {
    # ========== Error catching and default value definitions
    assertthat::assert_that(
      is.numeric(iter),
      "name" %in% names(resource.allocation),
      length(priority.values) == dim(W)[1],
      is.matrix(bigZ) && is.matrix(dt.bigZ),
      is.numeric(neighbors.T),
      # is.matrix(Y) && is.matrix(dt.Y),
      is.matrix(Y),
      is.matrix(W),
      # length(dt.dm) == dim(W)[1],
      # is.matrix(X) && is.matrix(dt.X)#,
      is.matrix(X)
      # is.numeric(newObj) && is.numeric(oldObj)
    )
    # ========== Call specific resource allocation strategy
    function_name <-
      paste0("resource_allocation_update_",
             tolower(resource.allocation$name))
    updt.args     <- as.list(sys.call())[-1]
    updated       <- do.call(function_name,
                             args = updt.args)
    priority.values <- updated$priority.values
    # dt.dm <- ifelse(test = resource.allocation$name == "RAD", yes = updated$dt.dm, no = dt.dm)
    # out <-
    #   list(priority.values = priority.values,
    #        dt.dm = dt.dm)

    out <-
      list(priority.values = priority.values)
    return(out)
  }
