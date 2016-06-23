# Generate a matrix of uniformly distributed values in the interval (0,1) with
# the same dimension as M
randM <- function(M) {
  if(is.matrix(M)) R <- matrix(runif(prod(dim(M))),
                               ncol = ncol(M))
  else if (is.vector(M)) R <- matrix(runif(length(M)),
                                     nrow = 1)
  else stop("M must be either a matrix or a vector")
  return(R)
}

# Denormalize population
denormalize_population <- function(probpars, Pop){
  # Denormalize population
  LL <- matrix(rep(probpars$xmin, nrow(Pop)),
               ncol = ncol(Pop),
               byrow = TRUE)
  UL <- matrix(rep(probpars$xmax, nrow(Pop)),
               ncol = ncol(Pop),
               byrow = TRUE)
  return(LL + Pop*(UL-LL))
}

# Get estimate of 'ideal' point (minP)
getminP <- function(X){
  apply(X, MARGIN = 2, FUN = min, na.rm = TRUE)
}

# Get estimate of 'nadir' point (maxP)
getmaxP <- function(X){
  apply(X, MARGIN = 2, FUN = max, na.rm = TRUE)
}

# Create a counter
# Function based on the simple counter by Hadley Wickham:
# http://adv-r.had.co.nz/Functional-programming.html
new_counter <- function(n0 = 0) {
  i <- n0
  function(n = 1) {
    force(n)
    i <<- i + n
    invisible(i)
  }
}
