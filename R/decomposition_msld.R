#' Problem Decomposition using Multi-layered Simplex-lattice Design
#'
#' Problem Decomposition using Multi-layered Simplex-lattice Design for MOEADr
#' package
#'
#' This routine calculates the weight vectors for the MOEA/D using the
#' Multi-layered Simplex-lattice Design.
#'
#' @param decomp list containing the relevant decomposition parameters.
#' For the Multi-layered Simplex-lattice design, the following key-value
#' pairs must be members of decomp:
#'
#' #' \tabular{ccc}{
#' \code{key}\tab \code{type}\tab \code{contents}\cr
#'
#' ".nobj" \tab Integer > 0 \tab Number of Objectives\cr
#' "H"    \tab Numeric Array \tab User parameter, h_i > 0\cr
#' "tau"    \tab Numeric Array \tab User parameter, |tau| = |H| and each tau_i must be unique\cr
#'}
#'
#'
#' @param ... other parameters (unused, included for compatibility with
#' generic call)
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
    all(sapply(decomp$H,assertthat::is.count)), ## there must be a cleaner way (all elements of H are count)
    assertthat::has_name(decomp,"tau"),
    all(decomp$tau >= 0 & decomp$tau < 1),
    assertthat::are_equal(decomp$tau,unique(decomp$tau)),
    assertthat::are_equal(length(decomp$H),length(decomp$tau)),
    assertthat::has_name(decomp,".nobj"),
    assertthat::is.count(decomp$.nobj),
    decomp$.nobj >= 2)


    W=1
    return(W)

    # assertthat::assert_that(
    #     assertthat::has_name(decopars, "H"),
    #     assertthat::has_name(decopars, "tau"))
    #
    # decopars2       <- decopars
    # decopars2$name  <- "Das"
    # decopars2$H     <- decopars2$H[1]
    #
    # W <- decompose_problem(decopars2, m)
    #
    # if (m > 6){
    #     assertthat::assert_that(length(decopars$H) == 2)
    #     decopars2$H <- decopars$H[2]
    #     W <- rbind(W,
    #                decompose_problem(decopars2, m) * decopars$tau +
    #                    (1 - decopars$tau) / m)
    # } else if (length(decopars$H) == 2){
    #     warning("Value of decopar$H[2] dropped.")
    # }
    # return (W)
}
