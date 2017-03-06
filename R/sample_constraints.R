#' Box constraints routine
#'
#' Calculates the constraint values and violations when only box constraints are
#' present.
#'
#' This routine calculates the constraint values and violations for a population
#' matrix in the MOEA/D. Each row of the matrix is considered as a candidate
#' solution. This routine expects the candidate solutions to be standardized,
#' i.e., that the variable limits given in \code{problem$xmin} and
#' \code{problem$xmax} are mapped to \code{0} and \code{1}, respectively.
#'
#' @param X Population matrix of the MOEA/D (each row is a candidate solution).
#' If \code{NULL} the function searches for \code{X} in the calling environment.
#' @param ... other parameters (unused, included for compatibility with
#' generic call)
#'
#' @return List objective containing a matrix of constraint values `Cmatrix`, a
#' matrix of individual constraint violations `Vmatrix`, and a vector of total
#' constraint violations `v`.
#'
#' @export
box_constraints <- function(X, ...){

  nv <- ncol(X) # number of problem variables

  # Prepare output matrix of constraint function values
  Cmatrix <- matrix(numeric(),
                    nrow = nrow(X),
                    ncol = 2 * nv) # 2 inequality box constraints/variable

  # Set informative column names (be nice to your users!)
  colnames(Cmatrix) <- paste0("x",
                              rep(1:nv, times = 2),
                              rep(c("min","max"), each = nv))

  # Box limits of the feasible space (remember, the population is considered as
  # standardized!)
  Xmin <- matrix(0, nrow = nrow(X), ncol = nv)
  Xmax <- matrix(1, nrow = nrow(X), ncol = nv)

  # Calculate "x_i >= 0" and "x_i <= 1" constraints
  Cmatrix[, 1:nv]              <- Xmin - X
  Cmatrix[, (nv + 1):(2 * nv)] <- X - Xmax

  # Assemble matrix of *violations*
  Vmatrix <- pmax(Cmatrix, 0)

  # Return necessary variables
  return(list(Cmatrix = Cmatrix,
              Vmatrix = Vmatrix,
              v       = rowSums(Vmatrix)))
}

#===================================================

#' Unitary constraints routine
#'
#' Calculates the constraint values and violations when only unitary constraints
#' (i.e., the sum of all variables equals one) are present.
#'
#' This routine calculates the constraint values and violations for a population
#' matrix in the MOEA/D. Each row of the matrix is considered as a candidate
#' solution. This routine expects the candidate solutions to be standardized,
#' i.e., that the variable limits given in \code{problem$xmin} and
#' \code{problem$xmax} are mapped to \code{0} and \code{1}, respectively.
#'
#' @param X Population matrix of the MOEA/D (each row is a candidate solution).
#' If \code{NULL} the function searches for \code{X} in the calling environment.
#' @param epsilon small non-negative value indicating the tolerance to be
#' considered for the equality constraint. Defaults to zero.
#' @param ... other parameters (unused, included for compatibility with
#' generic call)
#'
#' @return List objective containing a matrix of constraint values `Cmatrix`, a
#' matrix of individual constraint violations `Vmatrix`, and a vector of total
#' constraint violations `v`.
#'
#' @export
unitary_constraints <- function(X, epsilon = 0, ...){

  # Prepare output matrix of constraint function values
  Cmatrix <- matrix(numeric(),
                    nrow = nrow(X),
                    ncol = 1) # 1 equality constraint

  # Set informative column names (be nice to your users!)
  colnames(Cmatrix) <- "sum(x)-1"

  # Calculate equality constraint h = sum(x) - 1
  Cmatrix[, 1] <- rowSums(X) - 1

  # Assemble matrix of *violations*
  Vmatrix <- pmax(abs(Cmatrix) - epsilon, 0)

  # Return necessary variables
  return(list(Cmatrix = Cmatrix,
              Vmatrix = Vmatrix,
              v       = rowSums(Vmatrix)))
}
