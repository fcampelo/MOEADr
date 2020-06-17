decomposition_diverge <- function(decomp, ...) {
  # Error checking
  assertthat::assert_that(
    assertthat::has_name(decomp, "H"),
    assertthat::is.count(decomp$H),
    assertthat::has_name(decomp, ".nobj"),
    assertthat::is.count(decomp$.nobj),
    decomp$.nobj >= 2
  )
  
  H <- decomp$H
  m <- decomp$.nobj
  # Calculate number of weight vectors
  N <- choose(H + m - 1, m - 1)
  
  a <- 0.1
  W <- matrix(NA, nrow = N, ncol = m)
  for (i in 0:(N-1)) {
    term1 <- i / H
    term2 <- ((2 * i - H) * a) / H
    W[i + 1, 1] <- term1 + term2
    term1 <- (H - i) / H
    term2 <- ((2 * i - H) * a) / H
    W[i + 1, 2] <- term1 - term2
  }
  
  colnames(W) <- c("Var1","VarLast")
  return(W)
}
