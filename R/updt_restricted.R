#' Restricted Neighborhood Replacement Update for MOEA/D
#'
#' Population update using the restricted neighborhood replacement
#' method for the MOEADr package.
#'
#' The restricted neighborhood replacement method behaves like
#' the "standard" replacement method, except that each individual
#' can only be selected up to \code{nr} times. After this limit
#' has been reached, the next best individual in the same
#' neighborhood is selected.
#'
#' @section Parameters:
#'
#' This routine receives a single input variable, \code{moead.env}, which is
#' generated within the calling function \code{update_population()}. See
#' \code{\link{update_population}} for more information.
#'
#' @param moead.env list representing the environment of the base function
#' \code{moead}.
#'
#' This routine also expects the list \code{moead.env$updated} to be populated
#' with the following names:
#'
#' @param nr A positive integer. It determines the maximum number of times that
#' any individual can be selected (if nr is equal to the population size,
#' updated_restricted behaves exactly as updated_standard)
#'
#' @return List object containing the update population matrix (\code{Xnext})
#' and its corresponding matrix of objective function values (\code{Ynext}).
#'
#' @export

updt_restricted <- function(moead.env){

  assertthat::assert_that(
    assertthat::has_name(moead.env$update,"nr"),
    assertthat::is.count(moead.env$update$nr))

  nr <- moead.env$update$nr

  # Get the selection matrix for all neighborhoods
  sel.indx <- t(apply(moead.env$bigZ,
                      MARGIN = 2,
                      FUN = function (X) { unlist(as.matrix(sort.int(X, index.return = TRUE))[2]) }))
  # Code snipped for getting vector of sorting indexes from
  # https://joelgranados.com/2011/03/01/r-finding-the-ordering-index-vector/

  # Add a final column with the incumbent index
  sel.indx <- cbind(sel.indx,
                    rep(ncol(moead.env$B) + 1,
                        nrow(sel.indx)))

  # Function for returning the selected solution (variable or objectives space)
  # for a subproblem:
  # - i: subproblem index
  # - sel.indx: vector of selection indices (see above)
  # - XY: matrix of candidate solutions (in variable or objective space)
  # - XYt: matrix of incumbent solutions (in variable or objective space)
  # - B: matrix of neighborhoods
  do.update <- function(i, sel.indx, XY, XYt, B){
    for (j in sel.indx[i,]) {               #each element in b_i, in fitness order
      if (j > ncol(B)) return(XYt[i, ])     # last row = incumbent solution
      else if (used[B[i, j]] < nr)          # tests if the current element is still available
      {
        used[B[i, j]] <<- used[B[i, j]] + 1 # modifies count matrix in parent env
        return(XY[B[i, j], ])
      }
    }
  }

  # Counter of how many time each solution has been used
  used <- rep(0, nrow(moead.env$X))

  # Update matrix of candidate solutions
  Xnext <- t(vapply(X = 1:nrow(moead.env$X),
                    FUN = do.update,
                    FUN.VALUE = numeric(ncol(moead.env$X)),
                    sel.indx = sel.indx,
                    XY = moead.env$X,
                    XYt = moead.env$Xt,
                    B = moead.env$B,
                    USE.NAMES = FALSE))

  # Resetting counter for a second pass.
  used <- rep(0, nrow(moead.env$X))

  # Update matrix of function values
  Ynext <- t(vapply(X = 1:nrow(moead.env$Y),
                    FUN = do.update,
                    FUN.VALUE = numeric(ncol(moead.env$Y)),
                    sel.indx = sel.indx,
                    XY = moead.env$Y,
                    XYt = moead.env$Yt,
                    B = moead.env$B,
                    USE.NAMES = FALSE))

  # print(sum(used)) Total number of times the incumbent solution was NOT used, for debugging

  return(list(X = Xnext, Y = Ynext))
}
