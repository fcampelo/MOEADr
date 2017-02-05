#' Order Neighborhood for MOEA/D
#'
#' Calculates the ordering of competing solutions for each subproblem in the
#' MOEA/D, based on their scalarized performance and violation values.
#'
#' This routine receives a matrix of scalarized performance values (returned by
#' [scalarize_values()]), a neighborhood matrix, and a list of violation values,
#' and calculates the preference order of the candidates for each neighborhood
#' based on the performance values and the constraint handling method.
#'
#' @section Parameters:
#' This routine receives a single input variable, \code{moead.env}, which is
#' generated within the calling function [update_population()]. See that routine
#' for details.
#'
#' The environment listed in \code{moead.env} must contain the variable
#' \code{constraint}, which provides the type of constraint handling to be
#' employed, as well as its relevant parameters. The list of available
#' constraint handling methods can be generated using
#' [get_constraint_methods()].
#'
#' @param moead.env list representing the environment of the base function
#' [moead()].
#'
#' @return `[N x (T+1)]` matrix of preference indexes. Each row contains
#' the T indexes of the candidate solutions in the neighborhood of
#' a given subproblem, plus a value (column T+1) for the incumbent solution of
#' that subproblem, in an order defined by the constraint handling method
#' specified in \code{moead.env$constraint}.
#'
#' @export

order_neighborhood <- function(moead.env)
{

  # ========== Assert that moead.env has all necessary variables
  assertthat::assert_that(
    all(assertthat::has_name(moead.env, c("constraint", "bigZ",
                                          "B", "V", "Vt"))),
    nrow(moead.env$bigZ) == ncol(moead.env$B) + 1, # See issues. BigZ has strange dimensions compared to other matrices
    ncol(moead.env$bigZ) == nrow(moead.env$B))
  # ==========

  # If we don't have violations or constraint handling, just sort indexes by bigZ
  if (is.null(moead.env$V))
  {
    # Get the selection matrix for all neighborhoods using only bigZ
    sel.indx <- t(apply(moead.env$bigZ,
                        MARGIN = 2,
                        FUN = function (X) {
                          unlist(as.matrix(sort.int(X,
                                                    index.return = TRUE))[2])}))
    # Code snipped for getting vector of sorting indexes from
    # https://joelgranados.com/2011/03/01/r-finding-the-ordering-index-vector/
  }
  else
  {
    # calculate the penalty matrix of the neighborhoods and incumbent solution,
    # using the same process used to calculate bigZ (see scalarize_values)
    bigV <- t(cbind(matrix(moead.env$V$v[moead.env$B],
                           dim(moead.env$B)),
                    moead.env$Vt$v))

    # use constraint handler function to calculate selection matrix
    opname       <- paste0("constraint_", moead.env$constraint$name)

    # Update list of function inputs
    varargs      <- moead.env$constraint
    varargs$name <- NULL
    varargs$B    <- moead.env$B
    varargs$bigZ <- moead.env$bigZ
    varargs$bigV <- bigV
    varargs$V    <- moead.env$V$v
    varargs$Vt   <- moead.env$Vt$v

    # call constraint handling method
    sel.indx <- do.call(opname,
                        args = varargs)
  }

  return(sel.indx)
}
