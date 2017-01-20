#' Standard Neighborhood Replacement Update for MOEA/D
#'
#' Population update using the standard neighborhood replacement method for the
#' MOEADr package.
#'
#' This routine executes the standard neighborhood replacement operation to
#' update the population matrix of the MOEA/D.
#'
#' @section Parameters:
#' This routine receives a single input variable, \code{moead.env}, which is
#' generated within the calling function \code{update_population()}. See
#' \code{\link{update_population}} for more information.
#'
#' @param moead.env list representing the environment of the base function
#' \code{moead}.
#'
#' @return List object containing the update population matrix (\code{Xnext})
#' and its corresponding matrix of objective function values (\code{Ynext}).
#'
#' @export

updt_standard <- function(moead.env){
  # Solution x_i^{t+1} will receive the best solution from the set:
  # ${x_i^t, {v_j^t \forall j \in N(i)}} | w_i$
  # where $v_j^t$ is the j-th 'offspring' candidate solution, N(i) is the
  # neighborhood of i, and $w_i$ is the i-th weight vector.

  # ========== Error catching and default value definitions
  # Input "moead.env" is assumed to have been already verified in
  # update_population(), and will not be re-checked here.

  # Perform scaling and get updated estimate of the 'ideal' and 'nadir'
  # points
  normYs <- scale_objectives(moead.env)

  # Calculate matrix with scalarized performance values. Each column
  # contains the T scalarized performances of the candidate solutions in the
  # neighborhood of a given subproblem, plus the scalarized performance value
  # for the incumbent solution for that subproblem.
  bigZ <- scalarize_values(moead.env, normYs)

  # copy bigZ to the main environment "moead()" (for use with variation
  # operators, if needed)
  moead.env$bigZ <- bigZ

  # Get selection indices for each neighborhood
  sel.indx <- apply(bigZ,
                    MARGIN = 2,
                    FUN = which.min)

  # Function for returning the selected solution (variable or objectives space)
  # for a subproblem:
  # - i: subproblem index
  # - sel.indx: vector of selection indices (see above)
  # - XY: matrix of candidate solutions (in variable or objective space)
  # - XYt: matrix of incumbent solutions (in variable or objective space)
  # - B: matrix of neighborhoods
  do.update <- function(i, sel.indx, XY, XYt, B){
    if (sel.indx[i] > ncol(B)) return(XYt[i, ]) # last row = incumbent solution
    else return(XY[B[i, sel.indx[i]], ])
  }

  # Update matrix of candidate solutions
  Xnext <- t(vapply(X = 1:nrow(moead.env$X),
                    FUN = do.update,
                    FUN.VALUE = numeric(ncol(moead.env$X)),
                    sel.indx = sel.indx,
                    XY = moead.env$X,
                    XYt = moead.env$Xt,
                    B = moead.env$B,
                    USE.NAMES = FALSE))

  # Update matrix of function values
  Ynext <- t(vapply(X = 1:nrow(moead.env$Y),
                    FUN = do.update,
                    FUN.VALUE = numeric(ncol(moead.env$Y)),
                    sel.indx = sel.indx,
                    XY = moead.env$Y,
                    XYt = moead.env$Yt,
                    B = moead.env$B,
                    USE.NAMES = FALSE))

  return(list(X = Xnext, Y = Ynext))
}
