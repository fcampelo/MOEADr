#' Problem Decomposition using Uniform Design
#'
#' Problem Decomposition using Uniform Design for MOEADr
#' package
#'
#' This routine calculates the weight vectors for the MOEA/D using the
#' Uniform Design:
#'
#' @param decomp TODO
#' @param ... other parameters (unused, included for compatibility with
#' generic call)
#'
#' @section References:
#' R. Wang, T. Zhang, B. Guo, "An enhanced MOEA/D using uniform directions
#' and a pre-organization procedure". Proc. IEEE Congress on Evolutionary
#' Computation, Cancun, Mexico, 2013, pp. 2390–2397.
#' @export

decomposition_uniform <- function(decomp, ...){

  # Uniform requires that the number of objectives is at least 3
  assertthat::assert_that(
    assertthat::has_name(decomp, ".nobj"),
    assertthat::is.count(decomp$.nobj),
    decomp$.nobj >= 3)

  N <- decomp$.nobj

  # 1. Calculate the non-factors of N
  div <- seq_len(N);
  div <- div[N %% div != 0]

  # 2. Generates a matrix of size |N-1| subsets of div
  H <- t(sapply(seq_len(length(div)),
                function(x) {return(div[seq_len(length(div)) != x])}
        ))

  # QUESTION: Do I need the permutation of these subsets as well, or just the subsets?

  # 3. Generate matrixes U_N(h), and find one with lowest CD

  # 4. Generate weights from U_N(h) with lowest CD
}
