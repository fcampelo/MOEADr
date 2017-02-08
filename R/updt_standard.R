#' Standard Neighborhood Replacement Update for MOEA/D
#'
#' Population update using the standard neighborhood replacement method for the
#' MOEADr package.
#'
#' This routine executes the standard neighborhood replacement operation to
#' update the population matrix of the MOEA/D.
#' This update routine is intended to be used internally by the main [moead()]
#' function, and should not be called directly by the user.
#'
#' @param call.env list representing the environment of the base function
#' [moead()].
#'
#' @return List object containing the update population matrix (`X`),
#' and its corresponding matrix of objective function values (`Y`) and
#' constraint value list (`V`).
#'
#' @export

updt_standard <- function(call.env){
  # Solution x_i^{t+1} will receive the best solution from the set:
  # ${x_i^t, {v_j^t \forall j \in N(i)}} | w_i$
  # where $v_j^t$ is the j-th 'offspring' candidate solution, N(i) is the
  # neighborhood of i, and $w_i$ is the i-th weight vector.

  # Get best selection index for each neighborhood
  std.sel.indx <- call.env$sel.indx[, 1]

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
  Xnext <- t(vapply(X         = 1:nrow(call.env$X),
                    FUN       = do.update,
                    FUN.VALUE = numeric(ncol(call.env$X)),
                    sel.indx  = std.sel.indx,
                    XY        = call.env$X,
                    XYt       = call.env$Xt,
                    B         = call.env$B,
                    USE.NAMES = FALSE))

  # Update matrix of function values
  Ynext <- t(vapply(X         = 1:nrow(call.env$Y),
                    FUN       = do.update,
                    FUN.VALUE = numeric(ncol(call.env$Y)),
                    sel.indx  = std.sel.indx,
                    XY        = call.env$Y,
                    XYt       = call.env$Yt,
                    B         = call.env$B,
                    USE.NAMES = FALSE))


  # Update list of constraint values
  if(is.null(call.env$V)){
    Vnext <- NULL
  } else{
    Vnext <- list(Cmatrix = NULL, Vmatrix = NULL, v = NULL)

    ## 1: Cmatrix
    Vnext$Cmatrix <- t(vapply(X         = 1:nrow(call.env$V$Cmatrix),
                              FUN       = do.update,
                              FUN.VALUE = numeric(ncol(call.env$V$Cmatrix)),
                              sel.indx  = std.sel.indx,
                              XY        = call.env$V$Cmatrix,
                              XYt       = call.env$Vt$Cmatrix,
                              B         = call.env$B,
                              USE.NAMES = FALSE))
    ## 2: Vmatrix
    Vnext$Vmatrix <- t(vapply(X         = 1:nrow(call.env$V$Vmatrix),
                              FUN       = do.update,
                              FUN.VALUE = numeric(ncol(call.env$V$Vmatrix)),
                              sel.indx  = std.sel.indx,
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
