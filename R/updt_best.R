#' Best Neighborhood Replacement Update for MOEA/D
#'
#' Population update using the best neighborhood replacement method for the
#' MOEADr package.
#'
#' The Best Neighborhood Replacement method consists of three steps:
#'
#' \itemize{
#'     \item For each subproblem `i`, the best candidate solution `x_j` from the
#'           entire population is determined.
#'     \item The neighborhood of subproblem `i` is replaced by the neighborhood
#'     of subproblem j. The size of this neighborhood is given by a parameter
#'     `Tr`.
#'     \item The Restricted replacement (see [updt_restricted()]) is then
#'     applied using this new neighborhood.
#'}
#'
#' This update routine is intended to be used internally by the main [moead()]
#' function, and should not be called directly by the user.
#'
#' @param call.env list representing the environment of the base function
#' [moead()]. Variable `call.env$update` must have the following key-value
#' pairs:
#' \itemize{
#'   \item `update$Tr`: positive integer, neighborhood size for the update
#'   operation
#'   \item `update$nr`: positive integer, maximum number of copies of a given
#'   candidate solution.
#' }
#'
#' @return List object containing the update population matrix (`X`),
#' and its corresponding matrix of objective function values (`Y`) and
#' constraint value list (`V`).

updt_best <- function(call.env){

  ## Verify that the necessary parameters exist.
  assertthat::assert_that(
    all(assertthat::has_name(call.env$update, c("nr", "Tr"))),
    assertthat::is.count(call.env$update$nr),
    assertthat::is.count(call.env$update$Tr))

  nr <- call.env$update$nr
  Tr <- call.env$update$Tr

  # Calculate scalarized performance of all individuals for all subproblems
  fullZ   <- scalarize_values(normYs = call.env$normYs,
                              W      = call.env$W,
                              B      = call.env$BP$fullB,
                              aggfun = call.env$aggfun)

  # Find the problem in which each CANDIDATE solution (not incumbent) performs
  # best
  best.indx <- apply(X      = fullZ[1:(nrow(fullZ) - 1), ],
                     MARGIN = 1,
                     FUN    = which.min)

  best.subprob <- mapply(FUN      = function(i, j, B){B[i, j]},
                         i        = 1:nrow(call.env$BP$fullB),
                         j        = best.indx,
                         MoreArgs = list(B = call.env$BP$fullB))

  # Define restricted neighborhoods for best update (that is, the update
  # neighborhood of subproblem i is set as the neighborhood of best.subprob[i])
  bestB    <- call.env$BP$fullB[best.subprob, 1:Tr]

  # Assemble bigZ matrix according to neighborhood bestB
  bestZ <- scalarize_values(normYs = call.env$normYs,
                            W      = call.env$W,
                            B      = bestB,
                            aggfun = call.env$aggfun)

  best.sel.indx <- order_neighborhood(bigZ       = bestZ,
                                      B          = bestB,
                                      V          = call.env$V,
                                      Vt         = call.env$Vt,
                                      constraint = call.env$constraint)

  # ========= Code below here should be identical to updt_restricted =========#

  nr <- call.env$update$nr

  # Function for returning the selected solution (variable or objectives space)
  # for a subproblem:
  # - i: subproblem index
  # - sel.indx: matrix of selection indices
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

  # Vector of indices (random permutation), and deshuffling vector
  I  <- sample.int(nrow(call.env$X))
  I2 <- order(I)

  # Counter of how many time each solution has been used
  used <- rep(0, nrow(call.env$X))

  # Update matrix of candidate solutions
  Xnext <- t(vapply(X         = I,
                    FUN       = do.update,
                    FUN.VALUE = numeric(ncol(call.env$X)),
                    sel.indx  = best.sel.indx,
                    XY        = call.env$X,
                    XYt       = call.env$Xt,
                    B         = bestB,
                    USE.NAMES = FALSE))
  Xnext <- Xnext[I2, ]

  # Update matrix of function values
  used  <- rep(0, nrow(call.env$Y))
  Ynext <- t(vapply(X         = I,
                    FUN       = do.update,
                    FUN.VALUE = numeric(ncol(call.env$Y)),
                    sel.indx  = best.sel.indx,
                    XY        = call.env$Y,
                    XYt       = call.env$Yt,
                    B         = bestB,
                    USE.NAMES = FALSE))
  Ynext <- Ynext[I2, ]

  # Update list of constraint values
  Vnext <- list(Cmatrix = NULL, Vmatrix = NULL, v = NULL)

  ## 1: Cmatrix
  used <- rep(0, nrow(call.env$Y))
  Vnext$Cmatrix <- t(vapply(X         = I,
                            FUN       = do.update,
                            FUN.VALUE = numeric(ncol(call.env$V$Cmatrix)),
                            sel.indx  = best.sel.indx,
                            XY        = call.env$V$Cmatrix,
                            XYt       = call.env$Vt$Cmatrix,
                            B         = bestB,
                            USE.NAMES = FALSE))
  ## 2: Vmatrix
  used <- rep(0, nrow(call.env$Y))
  Vnext$Vmatrix <- t(vapply(X         = I,
                            FUN       = do.update,
                            FUN.VALUE = numeric(ncol(call.env$V$Vmatrix)),
                            sel.indx  = best.sel.indx,
                            XY        = call.env$V$Vmatrix,
                            XYt       = call.env$Vt$Vmatrix,
                            B         = bestB,
                            USE.NAMES = FALSE))

  ## 3: v
  Vnext$v <- rowSums(Vnext$Vmatrix)

  # Output
  return(list(X = Xnext,
              Y = Ynext,
              V = Vnext))
}
