variation_de <- function(X, P, phi = 0.5, ...) {
  # print((X))
  new.solution <- X
  dimX <- dim(X)[1]
  for (i in 1:dim(X)[1]) {
    idx <- sample.int(dimX, 3,
                   replace = TRUE,
                   prob    = P[, i])
    new.solution[i, ] <-
      X[idx[1], ] + phi * (X[idx[2], ] - X[idx[3], ])
  }
  return (new.solution)
}

# X <- lhs::randomLHS(10, 2)
# X
# variation_DE(X)
