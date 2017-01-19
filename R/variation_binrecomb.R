#' Binomial Recombination
#'
#' @param X Population matrix
#' @param rho mutation probability
#' @param Xc Original Population matrix
#' @param ... other parameters (unused, included for compatibility with
#' generic call)
#'
#' @return Matrix \code{X}' containing the mutated population
#'
#' @export

variation_binrecomb <- function(X, Xc, rho, ...){

  # ========== Error catching and default value definitions
  assertthat::assert_that(
    is.numeric(X) && is.matrix(X),
    is.numeric(Xc) && is.matrix(Xc),
    identical(dim(X), dim(Xc)),
    is.numeric(rho) && is_within(rho, 0, 1, strict = FALSE))
  # ==========

  # Define minimum mutating positions
  k <- matrix(FALSE, nrow(X), ncol(X))
  for (i in 1:nrow(X)) { k[i, sample(ncol(X), 1)] = TRUE }

  # Define mutation probability matrix (eq 28)
  u <- (randM(X) <= rho)

  # For any row without mutations, add the minimum mutation (eq 29)
  u <- u | (rowSums(u) == 0 & k)

  # Calculate the mutated population
  Xn <- X * (u) + Xc * (!u)

  return(Xn)
}
