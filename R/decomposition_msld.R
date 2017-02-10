#' Problem Decomposition using Multi-layered Simplex-lattice Design
#'
#' Problem Decomposition using Multi-layered Simplex-lattice Design for MOEADr
#' package
#'
#' This routine calculates the weight vectors for the MOEA/D using the
#' Multi-layered Simplex-lattice Design.
#'
#' @param decomp list containing the relevant decomposition parameters.
#'  Besides `decomp$name = "msld"`, this method requires the definition of the
#'  following key-value pairs in `decomp`:
#'
#' \itemize{
#'   \item `decomp$H`: array of positive integers representing the
#'                          \code{H} values to be used by the SLD decomposition
#'                          at each layer (see [decomposition_sld()] for
#'                          details).
#'   \item \code{decomp$tau}: array of scale multipliers for each layer,
#'         \eqn{0 < \tau_i \le 1}, \eqn{\tau_i != \tau_j} for all \eqn{i != j}.
#'         Must have the same lenght as \code{decomp$H}.
#'   \item \code{decomp$.nobj}: integer value, \code{decomp$.nobj > 1}. Number of
#'         objectives of the problem.
#' }
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @section References:
#' K. Li et al. (2014), "An Evolutionary Many-Objective Optimization
#' Algorithm Based on Dominance and Decomposition",
#' IEEE Trans. Evol. Comp. 19(5):694-716, 2015. DOI: 10.1109/TEVC.2014.2373386
#'
#' @export

decomposition_msld <- function(decomp, ...){

  # Validating parameters
  assertthat::assert_that(
    assertthat::has_name(decomp,"H"),
    length(decomp$H) > 1,
    all(sapply(decomp$H,assertthat::is.count)),
    assertthat::has_name(decomp,"tau"),
    is_within(decomp$tau, strict = c(TRUE, FALSE)), # is_within from utils.R
    assertthat::are_equal(decomp$tau,unique(decomp$tau)),
    assertthat::are_equal(length(decomp$H),length(decomp$tau)),
    assertthat::has_name(decomp,".nobj"),
    assertthat::is.count(decomp$.nobj),
    decomp$.nobj >= 2)

  # Calling SLD on each (h,tau) pair
  W <- mapply(decomp$H,
              decomp$tau,
              MoreArgs = list(decomp$.nobj),
              FUN = function(h, t, nobj) {
                # building parameter list for decomposition_sld
                x = list(H = h, .nobj = nobj)
                l = decomposition_sld(x)
                # scaling down vectors
                l = l * t + (1 - t) / nobj
                return(l)
              })

  # putting the results together and fixing funky rownames
  if (is.list(W)) {W <- do.call(rbind, W)}
  rownames(W) <- NULL

  return(W)
}
