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
#' [moead()] for details.
#' @param m Number of objectives (\eqn{m \ge 2})
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @return Weight matrix \code{W}
#'
#' @examples
#' decomp <- list(name = "sld", H = 99)
#' W <- generate_weights(decomp, m = 2)
#'
#' @export
#'
#' @section References:
#' F. Campelo, L.S. Batista, C. Aranha (2020): The {MOEADr} Package: A
#' Component-Based Framework for Multiobjective Evolutionary Algorithms Based on
#' Decomposition. Journal of Statistical Software \doi{10.18637/jss.v092.i06}\cr
#'

generate_weights <- function(decomp, m, ...)
{

  # ========== Error catching and default value definitions
  assertthat::assert_that(assertthat::is.count(m),
                          "name" %in% names(decomp))

  # Get the number of objectives into decomp
  decomp$.nobj <- m
  # ==========

  # ========== Generate vectors
  function_name <- paste0("decomposition_", tolower(decomp$name))
  W <- do.call(function_name,
               args = list(decomp = decomp))

  return(W)
}
