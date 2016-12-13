#' Run variation operators
#'
#' Sequentially apply variation operators for the MOEADr package
#'
#' This routine performs the variation block for the MOEA/D. The
#' list of available variation operators can be generated using
#' \code{get_variation_operators()}.
#'
#' @param X Population matrix of the MOEA/D (each row is a candidate solution).
#' If \code{NULL} the function searches for \code{X} in the calling environment.
#' @param P Matrix of probabilities of selection for variation (created by
#' \code{define_neighborhoods}). If \code{NULL} the function searches for
#' \code{P} in the calling environment.
#' @param variation List vector containing the variation operators to be used.
#' See \code{\link{moead}} for details. If \code{NULL} the function searches
#' for \code{variation} in the calling environment.
#'
#' @return Modified population matrix X
#'
#' @export

perform_variation <- function(X         = NULL,
                              P         = NULL,
                              variation = NULL){
  # Capture calling environment
  call.env <- parent.frame()

  # Capture "variation" from calling environment (if needed)
  if (is.null(variation)) {
    assertthat::assert_that(assertthat::has_name(call.env, "variation"))
    variation <- call.env$variation
  }

  # Capture "X" from calling environment (if needed)
  if (is.null(X)) {
    assertthat::assert_that(assertthat::has_name(call.env, "X"))
    X <- call.env$X
  }

  # Capture "P" from calling environment (if needed)
  if (is.null(P)) {
    assertthat::assert_that(assertthat::has_name(call.env, "P"))
    P <- call.env$P
  }

  # ========== Error catching and default value definitions
  # Assert that all elements of "variation" have a "name" field
  .ignore <- lapply(variation,
                    FUN = function(x){
                      assertthat::assert_that(assertthat::has_name(x, "name"))})

  for (i in seq_along(variation)){
    # Assemble function name
    opname       <- paste0("variation_", variation[[i]]$name)

    # Update list of function inputs
    varargs      <- variation[[i]]
    varargs$name <- NULL
    varargs$X    <- X
    varargs$P    <- P

    # Perform i-th variation operator
    X <- do.call(opname,
                 args = varargs)
  }

  return(X)
}
