update_moeadde <- function(env){
  # 'Offspring' candidate solution v_i^t will replace up to 'nr'
  # vectors x_k^t \in P, where P = N(i) if i \notin env$useWhole and
  # P = X otherwise. Vectors x_k^t \in P are compared to v_i^t in random order,
  # and replaced if (f(x_k^t) | w_k) > (f(v_i^t) | w_k)

  # To check: behavior

  # Population size
  np <- nrow(env$Y)

  # Get updated estimates of the 'ideal' and (if required) 'nadir' solutions
  minP <- with(env, getminP(rbind(Y, Yt)))

  if(env$scalpars$normalize.obj){
    maxP <- with(env, getmaxP(rbind(Y, Yt)))
  } else {
    maxP <- NULL
  }

  # Assemble list of valid neighbors for each position
  Nlist <- as.list(as.data.frame(t(env$N)), all.names = TRUE)

  if (any(names(env)=="useWhole")){
    Nlist <- lapply(X   = 1:length(Nlist),
                    FUN = function(x, Nl, U) {
                      if(any(x == U)) Nl[[x]] <- 1:length(Nl)
                      return(Nl[[x]])},
                    Nl = Nlist,
                    U  = env$useWhole)
  }

  # Vector of (sequential) neighbor positions
  Ninds <- unlist(Nlist)

  # Indices corresponding to each neighborhood
  Npos  <- rep(1:np, times = unlist(lapply(Nlist, length)))

  # Each 'offspring' is replicated by the size of its neighborhood
  bigY <- env$Y[Npos, ]

  # Match the weights of the neighborhood with the expanded offspring population
  bigW  <- env$W[Ninds, ]

  # Scalarize bigY by bigW and Yt by W
  bigZ <- scalarize_values(Y        = rbind(bigY, env$Yt),
                           W        = rbind(bigW, env$W),
                           scalpars = env$scalpars,
                           minP     = minP,
                           maxP     = maxP)

  # Separate current from candidate values
  Zt <- tail(bigZ, np)

  # Assemble data frame for comparisons and update
  bigU <- data.frame(home   = Npos,
                     target = Ninds,
                     Z      = head(bigZ, nrow(bigY)),
                     Zt     = Zt[Ninds])

  # Isolate positions in which the current value is improved by the candidate
  bigU <- subset(bigU, Z <= Zt)
  bigU <- bigU[sample.int(nrow(bigU)), ]

  # Initialize return population with current solutions
  Xnext <- env$Xt
  Ynext <- env$Yt

  if(nrow(bigU)){
    # Isolate 'nr' possible replacement positions for each candidate
    rePos <- lapply(unique(bigU$home),
                    FUN = sel_nr,
                    x   = bigU,
                    nr  = env$updtpars$nr)
    rePos <- do.call(rbind, rePos)

    # Isolate best 'candidate' for each position to be replaced
    rePos <- lapply(unique(rePos$target),
                    FUN = sel_best,
                    x   = rePos)
    rePos <- do.call(rbind, rePos)

    # Perform replacement
    Xnext[rePos$target, ] <- env$X[rePos$home, ]
    Ynext[rePos$target, ] <- env$Y[rePos$home, ]
  }

  return(list(X = Xnext, Y = Ynext))
}


## Auxiliary functions for updating

# Select up to 'nr' positions for each candidate
sel_nr <- function(i, x, nr){
  subset(x, home == i)[1:min(nr, length(which(x$home == i))), ]
}

# sel_nr2 <- function(i, x, nr){
#   x <- which(x == i)
#   x[1:min(nr, length(x))]
# }

# return best replacement for a given position
sel_best <- function(i, x){
  x <- subset(x, target == i)
  x[which.min(x$Z), ]
}
