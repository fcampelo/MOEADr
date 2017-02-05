#' Best Neighborhood Replacement Update for MOEA/D
#'
#' Population update using the best neighborhood replacement
#' method for the MOEADr package.
#'
#' The Best Neighborhood Replacement method consists of three steps:
#'
#' \itemize{
#'     \item For each subproblem i, the best candidate solution x_j from the
#'           entire population is determined.
#'     \item The neighborhood of subproblem i is replaced by the neighborhood of
#'           subproblem j. The size of this neighborhood, truncated by a
#'           parameter \code{Tr}.
#'     \item The Restricted replacement method is then applied using this new
#'     neighborhood.
#'}
#'
#' @section Parameters:
#' This routine receives a single input variable, \code{moead.env}, which is
#' generated within the calling function \code{update_population()}. It expects
#' \code{moead.env} to contain all fields listed in the \code{Parameters}
#' section of \code{\link{update_population}}. Additionaly, the parameter
#' \code{moead.env$update} is expected to contain two fields:
#' \itemize{
#'     \item \code{moead.env$update$Tr}  containing a positive integer.
#'     This value determines the size of the neighborhood to be used.
#'     \item \code{moead.env$update$nr} containing a positive integer.
#'           This value determines the maximum number of times that any
#'           candidate solution can be selected. If \code{nr = Tr}, then
#'           \code{update_restricted} is equivalent with \code{update_standard}.
#' }
#'
#' See \code{\link{update_population}} for more information.
#'
#' @param moead.env list representing the environment of the base function
#' \code{moead}. See section \code{Parameters} for details.
#'
#' @return List object containing the update population matrix (\code{Xnext})
#' and its corresponding matrix of objective function values (\code{Ynext}).
#'
#' @export

updt_best <- function(moead.env){

  ## Verify that the necessary parameters exist.
  assertthat::assert_that(
    all(assertthat::has_name(moead.env$update, c("nr", "Tr"))),
    assertthat::is.count(moead.env$update$nr),
    assertthat::is.count(moead.env$update$Tr))

  nr <- moead.env$update$nr
  Tr <- moead.env$update$Tr

  ## Generate expanded neighbor matrix
  # Preparing the environment requested by define_neighborhood()
  neighbors2   <- moead.env$neighbors
  neighbors2$T <- nrow(moead.env$X)
  iter         <- moead.env$iter
  X            <- moead.env$X
  W            <- moead.env$W

  if (!("bestB" %in% names(moead.env))) {
    # Calculate full neighborhood
    BP <- define_neighborhood(neighbors2)
    moead.env$bestB <- BP$B
    moead.env$bestP <- BP$P
  }
  B <- moead.env$bestB
  P <- moead.env$bestP

  # Calculate scalarized performance of all individuals for all subproblems
  bigZ   <- scalarize_values(moead.env, moead.env$normYs, B)

  # Find the problem in which each CANDIDATE solution (not incumbent) performs
  # best
  best.indx <- apply(X      = bigZ[1:(nrow(bigZ) - 1), ],
                     MARGIN = 1,
                     FUN    = which.min)

  best.subprob <- mapply(FUN      = function(i, j, B){B[i, j]},
                         i        = 1:nrow(B),
                         j        = best.indx,
                         MoreArgs = list(B = B))

  # Define restricted neighborhoods for best update (that is, the update
  # neighborhood of subproblem i is set as the neighborhood of best.subprob[i])
  B    <- B[best.subprob, 1:Tr]

  # Assemble bigZ matrix according to the update_best neighborhood
  bigZ     <- scalarize_values(moead.env, moead.env$normYs, B)
  best.env <- list (bigZ       = bigZ,
                    B          = B,
                    V          = moead.env$V,
                    Vt         = moead.env$Vt,
                    constraint = moead.env$constraint)
  sel.indx <- order_neighborhood(best.env)

  # ========= Code below here should be identical to updt_restricted =========#

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
                    B         = B,
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
                    B = B,
                    USE.NAMES = FALSE))

  # Unshuffle Xnext, Ynext
  I2 <- order(I)
  Xnext <- Xnext[I2, ]
  Ynext <- Ynext[I2, ]

  return(list(X = Xnext, Y = Ynext))
}
