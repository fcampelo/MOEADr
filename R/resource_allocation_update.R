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
           # newObj,
           # oldObj,
           ...) {
    # ========== Error catching and default value definitions
    assertthat::assert_that(
      is.numeric(iter),
      "name" %in% names(resource.allocation),
      length(priority.values) == dim(W)[1],
      is.matrix(bigZ) && is.matrix(dt.bigZ),
      is.numeric(neighbors.T),
      is.matrix(Y) && is.matrix(dt.Y),
      is.matrix(W),
      # length(dt.dm) == dim(W)[1],
      is.matrix(X) && is.matrix(dt.X)#,
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
