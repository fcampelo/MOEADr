resource_allocation_select <-
  function(iter,
           resource.allocation,
           W,
           priority.values,
           idx.boundary = NULL,
           two_step = NULL, 
           problem= NULL,
           ...) {
    # ========== Error catching and default value definitions
    assertthat::assert_that(
      is.numeric(iter),
      "selection" %in% names(resource.allocation),
      length(priority.values) == dim(W)[1],
      is.matrix(W),
      is.null(idx.boundary),
      is.null(two_step))
    
    
    # ========== Call specific resource allocation strategy
    function_name <- paste0("resource_allocation_select_", tolower(resource.allocation$selection))
    updt.args     <- as.list(sys.call())[-1]

    selected       <- do.call(function_name,
                             args = updt.args)
    indexes <- selected$indexes
    iteration_usage <- selected$iteration_usage
    
    
    out <-
      list(
        indexes = indexes,
        iteration_usage = iteration_usage
      )
    
    return(out)
  }



