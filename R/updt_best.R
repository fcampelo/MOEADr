#' "Best" Neighborhood Replacement Update for MOEA/D
#'
#' Population update using the "best neighborhood" replacement
#' method for the MOEADr package.
#'
#' The Best Neighborhood replacement method resolves in three
#' steps:
#'
#' 1- For each subproblem i, the best individual x_j from the
#'    entire population is chosen.
#'
#' 2- The neighborhood of subproblem i is replaced by the
#'    neighborhood of problem j. The size of this neighborhood
#'    is truncated by parameter \code{Tr}.
#'
#' 3- The Restricted replacement method is applied to this
#'    new neighborhood.
#'
#' @section Parameters:
#' This routine receives a single input variable, \code{moead.env}, which is
#' generated within the calling function \code{update_population()}. See
#' \code{\link{update_population}} for more information.
#'
#' @param moead.env list representing the environment of the base function
#' \code{moead}.
#'
#' @param nr A positive integer. It determinines the maximum number of times that
#' any individual can be selected (if nr is equal to the population size,
#' updated_restricted behaves exactly as updated_standard)
#'
#' @param Tr A positive integer. It determines the maximum size of the neighborhood
#' calculated by the Best Neighborhood method.
#'
#' @return List object containing the update population matrix (\code{Xnext})
#' and its corresponding matrix of objective function values (\code{Ynext}).
#'
#' @export

updt_best <- function(moead.env){

  ## Verify that the necessary parameters exist.
  assertthat::assert_that(
    assertthat::has_name(moead.env$update,"nr"),
    assertthat::has_name(moead.env$update,"Tr"),
    assertthat::is.count(moead.env$update$nr),
    assertthat::is.count(moead.env$update$Tr))

  nr <- moead.env$update$nr
  Tr <- moead.env$update$Tr

  ## Generate expanded neighbor matrix
  # Preparing the environment requested by define_neighborhood()
  neighbors <- moead.env$neighbors
  neighbors$T <- nrow(moead.env$X) - 1
  iter <- moead.env$iter
  X <- moead.env$X
  W <- moead.env$W
  if ("bestB" %in% names(moead.env)) {
    B <- moead.env$bestB
    P <- moead.env$bestP
  }

  # Calculating full neighborhood
  BP <- define_neighborhood()
  B <- BP$B
  P <- BP$P
  moead.env$bestB <- B
  moead.env$bestP <- P

  # Calculate performance of all individuals for all subproblems
  normYs <- scale_objectives(moead.env)
  bigZ <- scalarize_values(moead.env, normYs, B)

  # Find best solution for each problem and modify B and BigZ accordingly
  best.indx <- apply(bigZ,
                     MARGIN = 2,
                     FUN = which.min)
  best.cand <- sapply(1:nrow(B),FUN = function(X) { if (best.indx[X] == ncol(B)+1)
                                                       { X } else
                                                       { B[X, best.indx[X]] }})
  B <- B[best.cand, 1:Tr]
  bigZ <- scalarize_values(moead.env, normYs, B)

  # Code below here should be identical to updt_restricted

  # Get the selection matrix for all neighborhoods
  sel.indx <- t(apply(bigZ,
                      MARGIN = 2,
                      FUN = function (X) { unlist(as.matrix(sort.int(X, index.return = TRUE))[2]) }))
  # Code snipped for getting vector of sorting indexes from
  # https://joelgranados.com/2011/03/01/r-finding-the-ordering-index-vector/

  # Add a final column with the incumbent index
  sel.indx <- cbind(sel.indx, rep(ncol(B) + 1, nrow(sel.indx)))

  # Function for returning the selected solution (variable or objectives space)
  # for a subproblem:
  # - i: subproblem index
  # - sel.indx: vector of selection indices (see above)
  # - XY: matrix of candidate solutions (in variable or objective space)
  # - XYt: matrix of incumbent solutions (in variable or objective space)
  # - B: matrix of neighborhoods
  do.update <- function(i, sel.indx, XY, XYt, B){
    for (j in sel.indx[i,]) {               # each element in b_i, in fitness order
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
                    B = B,
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
                    B = B,
                    USE.NAMES = FALSE))

  return(list(X = Xnext, Y = Ynext))
}
