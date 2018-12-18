neighborhood_cga <- function(neighbors, v.matrix, iter, parallel = NULL, cl = NULL, ...) {
  BP <-
    list(
      B.variation = NULL,
      B.scalarize = NULL,
      B.order = NULL,
      P = NULL,
      fullB = NULL,
      fullP = NULL
    )
  # Calculate neighborhood matrix

  BP$fullB <- cbind(1:nrow(v.matrix),
                    FNN::get.knn(data      = v.matrix,
                                 k         = nrow(v.matrix) - 1)$nn.index)
  BP$B.variation <- BP$fullB[, 1:neighbors$T]
  BP$B.order <- BP$fullB[, 1:neighbors$LR]
  BP$B.scalarize <- BP$B.order

  np  <- nrow(v.matrix)

  BP$P   <- matrix((1 - neighbors$delta.p) / (np - neighbors$T),
                   nrow = np,
                   ncol = np)

  val <- neighbors$delta.p / neighbors$T
  BP$P   <- do.call(rbind,
                    lapply(1:np,
                           FUN = function(i, p, b, val){
                             p[i, b[i, ]] <- val; p[i, ]},
                           p   = BP$P,
                           b   = BP$B,
                           val = val))

  BP$fullP <- BP$P
  BP$fullP[, ] <- 1 / ncol(BP$fullP)
  return(BP)
}
