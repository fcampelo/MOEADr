#' Problem Decomposition using Multi-layered Simplex-lattice Design
#'
#' Problem Decomposition using Multi-layered Simplex-lattice Design for MOEADr
#' package
#'
#' This routine calculates the weight vectors for the MOEA/D using the
#' Multi-layered Simplex-lattice Design.
#'
#' @param decomp TODO
#'
#' @section References:
#' K. Li et al. (2014), "An Evolutionary Many-Objective Optimization
#' Algorithm Based on Dominance and Decomposition",
#' IEEE Trans. Evol. Comp. 19(5):694-716, 2015. DOI: 10.1109/TEVC.2014.2373386

decomposition_msld <- function(decomp){

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
