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
#' This routine expects \code{moead.env} to contain all fields listed in the
#' \code{Parameters} section of \code{\link{update_population}}. Additionaly,
#' the parameter \code{moead.env$update} is expected to contain a
#' field \code{moead.env$update$nr} containing a positive integer.
#' This value determines the maximum number of times that
#' any individual can be selected.
#'
#' @return List object containing the updated population matrix (\code{Xnext})
#' and its corresponding matrix of objective function values (\code{Ynext}).
#'
#' @export

updt_restricted <- function(moead.env){

  # ========== Error catching and default value definitions
  assertthat::assert_that(
    assertthat::has_name(moead.env$update,"nr"),
    assertthat::is.count(moead.env$update$nr),
    assertthat::has_name(moead.env,"sel.indx"))

  nr <- moead.env$update$nr
  sel.indx <- moead.env$sel.indx

  # Function for returning the selected solution (variable or objectives space)
  # for a subproblem:
  # - i: subproblem index
  # - sel.indx: matrix of selection indices (see above)
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

  # Vector of indices (random permutation)
  I  <- sample.int(nrow(moead.env$X))

  # Counter of how many time each solution has been used
  used <- rep(0, nrow(moead.env$X))

  # Update matrix of candidate solutions
  Xnext <- t(vapply(X         = I,
                    FUN       = do.update,
                    FUN.VALUE = numeric(ncol(moead.env$X)),
                    sel.indx  = sel.indx,
                    XY        = moead.env$X,
                    XYt       = moead.env$Xt,
                    B         = moead.env$B,
                    USE.NAMES = FALSE))

  # Resetting counter for a second pass.
  used <- rep(0, nrow(moead.env$X))

  # Update matrix of function values
  Ynext <- t(vapply(X = I,
                    FUN = do.update,
                    FUN.VALUE = numeric(ncol(moead.env$Y)),
                    sel.indx = sel.indx,
                    XY = moead.env$Y,
                    XYt = moead.env$Yt,
                    B = moead.env$B,
                    USE.NAMES = FALSE))

  # Unshuffle Xnext, Ynext
  I2 <- order(I)
  Xnext <- Xnext[I2, ]
  Ynext <- Ynext[I2, ]

  return(list(X = Xnext, Y = Ynext))
}
