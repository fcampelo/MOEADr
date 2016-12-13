#' Calculate weight vectors
#'
#' Calculates weight vectors for the MOEADr package
#'
#' This routine calculates the weight vectors for the MOEA/D. The
#' list of available methods for generating the weights, as well as information
#' about their specific parameters, can be generated using
#' \code{get_decomposition_methods()}.
#'
#' @param decomp List containing the decomposition method parameters. See
#' \code{\link{moead}} for details. If \code{NULL} the function searches for
#' \code{decomp} in the calling environment.
#' @param m Number of objectives (\eqn{m \ge 2})
#'
#' @return Weight matrix \code{W}
#'
#' @export

generate_weights <- function(decomp = NULL, m)
{
  # Capture calling environment
  call.env <- parent.frame()

  # Capture "decomp" from calling environment (if needed)
  if (is.null(decomp)) {
    assertthat::assert_that(assertthat::has_name(call.env, "decomp"))
    decomp <- call.env$decomp
  }

  decomp$.nobj <- m

  # ========== Error catching and default value definitions
  assertthat::assert_that(assertthat::is.count(m))


  # ========== Generate vectors
  function_name <- paste0("decomposition_", tolower(decomp$name))
  W <- do.call(function_name,
               args = list(decomp = decomp))

  return(W)
}
