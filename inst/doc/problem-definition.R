## ---- eval = FALSE-------------------------------------------------------
#  problem <- list(name        = "moeadr_dtlz1",  # objective function routine
#                  xmin        = rep(0, 10),      # lower limits
#                  xmax        = rep(1, 10),      # upper limits
#                  m           = 2,               # number of objectives
#                  constraints = list(
#                    name      = "my_constraints",# constraint function routine
#                    epsilon   = 0.05))           # tolerance for equality constraints

## ---- eval = FALSE-------------------------------------------------------
#  moeadr_dtlz1 <- function(X,     # population matrix
#                           ...    # allow function to receive extra parameters.
#                                  # These are unused in most cases, but it is useful
#                                  # for preventng errors due to unwanted parameters
#                                  # being passed
#  ){
#  
#    # "smoof" is listed in the Suggests field MOEADr's DESCRIPTION, but we need to
#    # be sure that it is available, so:
#    if(!("smoof" %in% rownames(utils::installed.packages()))){
#      stop("Please install package 'smoof' to continue")
#    }
#  
#    # make 10-variable, 2-objective DTLZ1
#    smoof_dtlz1 <- smoof::makeDTLZ1Function(dimensions   = 10,
#                                            n.objectives = 2)
#  
#    # Evaluate points in a vectorized manner:
#    Y <- t(apply(X,
#                 MARGIN = 1,
#                 FUN = smoof_dtlz1))
#  
#    # Return [N x n_f] matrix
#    return(Y)
#  }

## ---- eval = FALSE-------------------------------------------------------
#  my_constraints <- function(X,           # population matrix
#                             epsilon = 0, # tolerance for equality constraints
#                                          # (defaults to zero if not provided)
#                             ...)
#  {
#  
#    nv <- 10 # number of variables of the problem
#  
#    # Prepare output matrix of constraint function values
#    Cmatrix <- matrix(numeric(),
#                      nrow = nrow(X),
#                      ncol = 2 * nv + 2) # 20 inequality box constraints, plus g1 and h1
#  
#    # Set informative column names (be nice to your users!)
#    colnames(Cmatrix) <- c(paste0("x",
#                                  rep(1:nv, times = 2),
#                                  rep(c("min","max"), each = nv)),
#                           "g1",
#                           "h1")
#  
#    # Box limits of the feasible space
#    Xmin <- matrix(0, nrow = nrow(X), ncol = nv)
#    Xmax <- matrix(1, nrow = nrow(X), ncol = nv)
#  
#    # Calculate "x_i >= 0" and "x_i <= 1" constraints
#    Cmatrix[, 1:nv]              <- Xmin - X
#    Cmatrix[, (nv + 1):(2 * nv)] <- X - Xmax
#  
#    # g1 and h1 functions
#    g1 <- function(X){
#      return(X[, 1] ^ 2 + 2 * X[, 2] ^ 2 - 1.2)
#    }
#    h1 <- function(X){
#      return(X[, 3] * X[, 4] - 0.5)
#    }
#  
#    # Calculate g1(x) and h1(x)
#    Cmatrix[, 2 * nv + 1] <- g1(X)
#    Cmatrix(, 2 * nv + 2) <- h1(X)
#  
#    # Assemble matrix of *violations*
#    Vmatrix <- Cmatrix
#    Vmatrix[, 1:(2 * nv + 1)] <- pmax(Vmatrix[, 1:(2 * nv + 1)], 0)        # inequality constraints
#    Vmatrix[, 2 * nv + 2] <- pmax(abs(Vmatrix[, 2 * nv + 2]) - epsilon, 0) # equality constraint h1
#  
#    # Return necessary variables
#    return(list(Cmatrix = Cmatrix,
#                Vmatrix = Vmatrix,
#                v       = rowSums(Vmatrix)))
#  
#  }

