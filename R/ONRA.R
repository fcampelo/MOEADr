ONRA <- function(dt.bigZ, bigZ, epsilon=1e-50) {
  dt <- apply(X = dt.bigZ, MARGIN = 2, sum)
  z <- apply(X = bigZ, MARGIN = 2, sum)
  u <- (dt - z) / dt
  if(max(u) == 0) {
    p <- rep(1, length(u))
  }
  else{
    p <- (u + epsilon) / (max(u) + epsilon)
  }
  return(p)
}


