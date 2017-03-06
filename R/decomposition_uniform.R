#' Problem Decomposition using Uniform Design
#'
#' Problem Decomposition using Uniform Design for MOEADr package
#'
#' This routine calculates the weight vectors for the MOEA/D using the Uniform
#' Design:
#'
#' @param decomp list containing the relevant decomposition parameters.
#'  Besides `decomp$name = "uniform"`, this method requires the definition of the
#'  following key-value pairs:
#' \itemize{
#'   \item `decomp$N`, number of subproblems to generate. It is important to
#'         highlight that the number of subproblems must be greater than the
#'         number of neighbors declared in `neighbors$T` (see [moead()] for
#'         details).
#'   \item \code{decomp$.nobj}: integer value, `decomp$.nobj > 1`. Number of
#'         objectives of the problem.
#' }
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @section References:
#' R. Wang, T. Zhang, B. Guo, "An enhanced MOEA/D using uniform directions
#' and a pre-organization procedure". Proc. IEEE Congress on Evolutionary
#' Computation, Cancun, Mexico, 2013, pp. 2390–2397.

# ==========
# RETURN TO DOCUMENTATION AFTER FIXING
# @export
# ==========

decomposition_uniform <- function(decomp, ...){

  # nobj - number of objectives (problem defined)
  # N - number of subproblems (user defined)
  assertthat::assert_that(
    assertthat::has_name(decomp, "N"),
    assertthat::is.count(decomp$N),
    assertthat::has_name(decomp, ".nobj"),
    assertthat::is.count(decomp$.nobj),
    decomp$N >= 3)

  N  <- decomp$N
  nf <- decomp$.nobj

  # 1. Calculate the non-factors of N
  div <- which(sapply(seq_len(N), is_coprime, N))

  # 2. Generates H, matrix of all |nf-1| size subsets of div
  H <- t(utils::combn(x = div, m = nf-1))

  # 3. Generate matrixes U_N(h), and find one with lowest CD
  construct_un <- function(h, N) {
    U <- t(sapply(seq_len(N), function(x) {(h * x) %% N}))

    # Authors redefine modulus as going from 1 to N
    U <- U + N * (1 - sign(U))
  }

  cd2 <- function(U) {
    # L2 discrepancy of U (equation 7)
    magic <- (13 / 12) ^ ncol(U)

    S1 <- (2 / nrow(U)) *
          sum(apply((1 + (abs(U - 0.5) - abs(U - 0.5) ^ 2) / 2), 1, prod))

    S2 <- (1 / nrow(U) ^ 2) *
          sum(sapply(1:nrow(U), function(y) { tU <- matrix(U[y, ],
                                                           nrow  = nrow(U),
                                                           ncol  = ncol(U),
                                                           byrow = TRUE)
                                              apply(1 + (abs(U - 0.5) + abs(tU - 0.5)) / 2
                                                      - abs(U - tU) / 2,
                                                    MARGIN = 1,
                                                    FUN    = prod)}))

    return (magic - S1 + S2)
  }

  min_h <- H[which.min(apply(H,
                             MARGIN = 1,
                             FUN    = function(x) {cd2(construct_un(x, N))})), ]
  Un    <- (construct_un(min_h, N) - 0.5) / N

  if (nrow(Un) == 1) { Un <- t(Un) } # Fixes R arbitrarily choosing column major when (nf = 2) (iss 34)


  # 4. Generate weights from U_N(h)

  U_pow <- t(t(Un) ^ sapply(seq_len(nf - 1),
                            function(x) {(nf - x) ^ -1}))
  pow_prod <- t(apply(U_pow,
                      MARGIN = 1,
                      FUN    = function(x) {
                        sapply(seq_len(length(x)),
                               FUN = function(y) { prod(x[seq_len(y-1)])})}))

  if (nrow(pow_prod) == 1) { pow_prod <- t(pow_prod) }
  # Fixes R arbitrarily choosing column major when (nf = 2) (iss 34)

  W <- (1 - U_pow) * pow_prod
  colnames(W) <- paste("Var", 1:ncol(W), sep="")

  # Adding the final Column and returning
  return(cbind(W, VarLast = apply(U_pow,
                                  MARGIN = 1,
                                  FUN    = prod)))

}
