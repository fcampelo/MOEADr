#' Restricted Neighborhood Replacement Update for MOEA/D
#'
#' Population update using the restricted neighborhood replacement
#' method for the MOEADr package.
#'
#' The restricted neighborhood replacement method behaves like the "standard"
#' replacement method, except that each individual can only be selected up to
#' `nr` times. After this limit has been reached, the next best individual in
#' the same neighborhood is selected.
#'
#' This update routine is intended to be used internally by the main [moead()]
#' function, and should not be called directly by the user.
#'
#' @param call.env list representing the environment of the base function
#' [moead()]. Variable `call.env$update` must have a field `update$nr`
#' containing a positive integer (maximum number of copies of a given candidate
#' solution)
#'
#' @return List object containing the update population matrix (`X`),
#' and its corresponding matrix of objective function values (`Y`) and
#' constraint value list (`V`).
#'
#' @export
updt_restricted <- function(call.env){

  # ========== Error catching and default value definitions
  assertthat::assert_that(
    assertthat::has_name(call.env$update,"nr"),
    assertthat::is.count(call.env$update$nr))

  nr            <- call.env$update$nr
  rest.sel.indx <- call.env$sel.indx

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
                    sel.indx  = rest.sel.indx,
                    XY        = call.env$X,
                    XYt       = call.env$Xt,
                    B         = call.env$B,
                    USE.NAMES = FALSE))
  Xnext <- Xnext[I2, ]

  # Update matrix of function values
  used <- rep(0, nrow(call.env$Y))
  Ynext <- t(vapply(X         = I,
                    FUN       = do.update,
                    FUN.VALUE = numeric(ncol(call.env$Y)),
                    sel.indx  = rest.sel.indx,
                    XY        = call.env$Y,
                    XYt       = call.env$Yt,
                    B         = call.env$B,
                    USE.NAMES = FALSE))
  Ynext <- Ynext[I2, ]

  # Update list of constraint values
  if(is.null(call.env$V)){
    Vnext <- NULL
  } else{
    Vnext <- list(Cmatrix = NULL, Vmatrix = NULL, v = NULL)

    ## 1: Cmatrix
    used <- rep(0, nrow(call.env$Y))
    Vnext$Cmatrix <- t(vapply(X         = I,
                              FUN       = do.update,
                              FUN.VALUE = numeric(ncol(call.env$V$Cmatrix)),
                              sel.indx  = rest.sel.indx,
                              XY        = call.env$V$Cmatrix,
                              XYt       = call.env$Vt$Cmatrix,
                              B         = call.env$B,
                              USE.NAMES = FALSE))
    ## 2: Vmatrix
    used <- rep(0, nrow(call.env$Y))
    Vnext$Vmatrix <- t(vapply(X         = I,
                              FUN       = do.update,
                              FUN.VALUE = numeric(ncol(call.env$V$Vmatrix)),
                              sel.indx  = rest.sel.indx,
                              XY        = call.env$V$Vmatrix,
                              XYt       = call.env$Vt$Vmatrix,
                              B         = call.env$B,
                              USE.NAMES = FALSE))

    ## 3: v
    Vnext$v <- rowSums(Vnext$Vmatrix)
  }

    # Output
  return(list(X = Xnext,
              Y = Ynext,
              V = Vnext))
}
