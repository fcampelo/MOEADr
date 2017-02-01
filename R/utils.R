# Generate an matrix of uniformly distributed values in the interval (0,1) with
# the same dimension as M
randM <- function(M) {
  matrix(stats::runif(prod(dim(M))),
         ncol = ncol(M))
}

# Denormalize population
# X is a matrix of row vectors
denormalize_population <- function(X, problem){
  # Denormalize population
  LL <- matrix(rep(problem$xmin, nrow(X)),
               ncol  = ncol(X),
               byrow = TRUE)
  UL <- matrix(rep(problem$xmax, nrow(X)),
               ncol  = ncol(X),
               byrow = TRUE)
  return(LL + X * (UL - LL))
}



# Check if a numeric value is within certain bounds
is_within <- function(x, xmin = 0, xmax = 1, strict = c(FALSE, FALSE)){
  if(length(strict) == 1) strict <- rep(strict, 2)
  out <- is.numeric(x) &
    ifelse(strict[1],
           all(x >  xmin),
           all(x >= xmin)) &
    ifelse(strict[2],
           all(x <  xmax),
           all(x <= xmax))
  return(out)
}

# Check if two numeric values are coprime
is_coprime <- function(x, y){
  a <- x
  b <- y
  while (b != 0) {
    if (a == 1 || b == 1)
      return(TRUE)
    t <- b
    b <- a %% b
    a <- t
  }
  return(FALSE)
}

# Get estimate of 'ideal' point (minP)
getminP <- function(X){
  apply(X, MARGIN = 2, FUN = min, na.rm = TRUE)
}



# Get estimate of 'nadir' point (maxP)
getmaxP <- function(X){
  apply(X, MARGIN = 2, FUN = max, na.rm = TRUE)
}

# Example of violation condition: Box constraints.
# Sum the values from a solution matrix that are out of bound of 0 and 1
violation_box <- function(X,...) { apply(X, 1, function(x) { sum((pmax(x - 1, 0)) + abs(pmin(x, 0)))} )}

# Example of violation condition: Unitary constraints.
# Calculate the difference between the L1 norm of each individual and 1
# (in other words, whether the parameters sum to 1 or not)
violation_unitary <- function(X,...) { apply(X, 1, function(x) { abs(sum(x) - 1) }) }

