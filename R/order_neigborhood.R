

order_neighborhood <- function(moead.env)
{

  # ========== Assert that moead.env has all necessary variables
  assertthat::assert_that(
    all(assertthat::has_name(moead.env, c("bigZ", "B", "V", "Vt"))),
    nrow(moead.env$bigZ) == ncol(moead.env$B)+1, # See issues. BigZ has strange dimensions compared to other matrices
    ncol(moead.env$bigZ) == nrow(moead.env$B))
  # ==========

  # If we don't have violations or constraint handling, just sort indexes by bigZ
  if (is.null(moead.env$V) || !"constraint" %in% names(moead.env))
  {
    # Get the selection matrix for all neighborhoods
    sel.indx <- t(apply(moead.env$bigZ,
                        MARGIN = 2,
                        FUN = function (X) { unlist(as.matrix(sort.int(X, index.return = TRUE))[2]) }))
    # Code snipped for getting vector of sorting indexes from
    # https://joelgranados.com/2011/03/01/r-finding-the-ordering-index-vector/
  }
  else
  {
    # calculate the penalty matrix of the neighborhoods and incumbent
    bigV <- t(cbind(matrix(V[B],dim(B)),Vt))

    # use constraint handler function to calculate selection matrix
    opname       <- paste0("constraint_", moead.env$constraint$name)

    # Update list of function inputs
    varargs      <- moead.env$constraint
    varargs$name <- NULL
    varargs$B    <- moead.env$B
    varargs$bigZ <- moead.env$bigZ
    varargs$bigV <- bigV
    # Perform i-th variation operator

    sel.indx <- do.call(opname,
                        args = varargs)
  }

  return(sel.indx)
}
