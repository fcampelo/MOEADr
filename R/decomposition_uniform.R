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

  # nobj - number of objectives (problem defined)
  # N - number of subproblems (user defined)
  assertthat::assert_that(
    assertthat::has_name(decomp, "N"),
    assertthat::is.count(decomp$N),
    assertthat::has_name(decomp, ".nobj"),
    assertthat::is.count(decomp$.nobj),
    decomp$N >= 3)

  N <- decomp$N
  nf <- decomp$.nobj

  # 1. Calculate the non-factors of N
  # div <- seq_len(N);
  # div <- div[N %% div != 0]

  div <- seq_len(N)
  div <- div[sapply(div, is_coprime, N)]

  # 2. Generates H, matrix of all |nf-1| size subsets of div
  # Question: Are the permutations of the rows of H needed?
  H <- t(combn(div, nf-1))

  # 3. Generate matrixes U_N(h), and find one with lowest CD
  construct_un <- function(h, N) {
    U <- t(sapply(seq_len(N), function(x) {(h * x)%% N}))

    # Authors redefine modulus as going from 1 to N
    U <- U + N * (1 - sign(U))
  }

  cd2 <- function(U) {
    # L2 discrepancy of U (equation 7)
    magic <- (13 / 12) ^ ncol(U)
    S1 <- (2 / nrow(U)) *
          sum(apply((1 + (abs(U - 0.5) - abs(U - 0.5) ^ 2) / 2), 1, prod))

    # FIXME: This line looks cool, but it is too slow. Replace with an ugly 3-loop
    S2 <- (1 / nrow(U) ^ 2) *
          sum(apply(expand.grid(1:nrow(U), 1:nrow(U), 1:ncol(U)),
                                1,
                                function(x) { 1 +
                                              abs(U[x[1], x[3]] - 0.5) / 2 +
                                              abs(U[x[2], x[3]] - 0.5) / 2 -
                                              abs(U[x[1], x[3]] - U[x[2], x[3]]) / 2
          }))
    return (magic - S1 + S2)
  }

  min_h <- H[which.min(apply(H, 1, function(x) {cd2(construct_un(x, N))})), ]
  Un <- (construct_un(min_h, N) - 0.5) / N

  # 4. Generate weights from U_N(h)

  U_pow <- t(t(Un) ^ sapply(seq_len(nf - 1), function(x) {(nf - x) ^ -1}))
  pow_prod <- t(apply(U_pow, 1,
                      function(x) {sapply(seq_len(length(x)),
                                          function(y) { prod(x[seq_len(y-1)]) }
                )}))

  W <- (1 - U_pow) * pow_prod
  colnames(W) <- paste("Var", 1:ncol(W), sep="")

  # Adding the final Column
  W <- cbind(W, VarLast = apply(U_pow, 1, prod))

}
