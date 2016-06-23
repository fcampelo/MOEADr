update_standard <- function(env){
  # Solution x_i^{t+1} will receive the best solution from the set:
  # ${x_i^t, {v_j^t \forall j \in N(i)}} | w_i$
  # where $v_j^t$ is the j-th 'offspring' candidate solution, N(i) is the
  # neighborhood of i, and $w_i$ is the i-th weight vector.

  # Get updated estimate of the 'ideal' solution
  minP <- getminP(rbind(env$Y, env$Yt))

  if(env$scalpars$normalize.obj || env$scalpars$name == "ipbi"){
    maxP <- getmaxP(rbind(env$Y, env$Yt))
  } else {
    maxP <- NULL
  }

  bigY <- env$Y[as.vector(t(env$N)), ]

  bigW <- env$W[rep(1:nrow(env$W),
                each  = ncol(env$N)), ]
  bigZ <- matrix(numeric(),
                 ncol = nrow(env$Y),
                 nrow = ncol(env$N) + 1)
  bigZ[1:ncol(env$N), ] <- matrix(scalarize_values(Y        = bigY,
                                                   W        = bigW,
                                                   scalpars = env$scalpars,
                                                   minP     = minP,
                                                   maxP     = maxP),
                                  ncol  = nrow(env$W),
                                  byrow = FALSE)
  bigZ[nrow(bigZ), ] <- scalarize_values(Y        = env$Yt,
                                         W        = env$W,
                                         scalpars = env$scalpars,
                                         minP     = minP,
                                         maxP     = maxP)

  sel.indx <- apply(bigZ,
                    MARGIN = 2,
                    FUN = which.min)

  do.update <- function(i, sel.indx, K, Kt, Ni){
    if (sel.indx[i] > ncol(Ni)) return(Kt[i, ])
    else return(K[Ni[i, sel.indx[i]], ])
  }

  Ynext <- t(vapply(X = 1:nrow(env$Y),
                    FUN = do.update,
                    FUN.VALUE = numeric(ncol(env$Y)),
                    sel.indx = sel.indx,
                    K = env$Y,
                    Kt = env$Yt,
                    Ni = env$N,
                    USE.NAMES = FALSE))

  Xnext <- t(vapply(X = 1:nrow(env$X),
                    FUN = do.update,
                    FUN.VALUE = numeric(ncol(env$X)),
                    sel.indx = sel.indx,
                    K = env$X,
                    Kt = env$Xt,
                    Ni = env$N,
                    USE.NAMES = FALSE))

  return(list(X = Xnext, Y = Ynext))
}
