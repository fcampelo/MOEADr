# Objecive Function for the number of continuous shade days
ContinuousShadeDays <- function(X){
  
  write(X,file = paste(getwd(), "evaluate/pop_vars_eval.txt", sep="/"), ncolumns = n_variables, sep = "\t")
  system(paste(paste(getwd(), "moon_mop", sep = "/"), paste(getwd(), "evaluate/", sep = "/"), sep = " "), ignore.stdout = TRUE)
  objectives <- scan(paste(getwd(), "evaluate/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = 3, byrow = TRUE)
  
  weight = matrix(objectives[,1], ncol = 1)
  weight
}

# Objecive Function for the total communication time
TotalCommunicationTime <- function(X){
  
  objectives <- scan(paste(getwd(), "evaluate/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = 3, byrow = TRUE)
  
  weight = matrix(objectives[,2], ncol = 1)
  weight
}

# Objecive Function for the inclination angle
InclinationAngle <- function(X){
  
  objectives <- scan(paste(getwd(), "evaluate/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = 3, byrow = TRUE)
  
  weight = matrix(objectives[,3], ncol = 1)
  weight
}

# Definition of the problem
problem.moon <- function(X) {
  t(apply(X, MARGIN = 1,
          FUN = function(X) { c(ContinuousShadeDays(X), TotalCommunicationTime(X), InclinationAngle(X)) }
  ))
}

my_constraints <- function(X)
{
  nv <- n_variables # number of variables
  # Prepare output matrix of constraint function values
  Cmatrix <- matrix(numeric(),
                    nrow = nrow(X),
                    ncol = 2 * nv + n_constraints) # 296 box constraints and 36 inequality constraints
  
  # Set informative column names (be nice to your users!)
  colnames(Cmatrix) <- c(paste0("x",
                                rep(1:nv, times = 2),
                                rep(c("min","max"), each = nv)),
                         rep(c("g1"), each = n_constraints))
  
  # Box limits of the feasible space
  Xmin <- matrix(minimum, ncol = n_variables, nrow = nrow(X), byrow = TRUE)
  Xmax <- matrix(maximum, ncol = n_variables, nrow = nrow(X), byrow = TRUE)
  
  # Calculate "x_i >= 0" and "x_i <= 1" constraints
  Cmatrix[, 1:nv]              <- Xmin - X
  Cmatrix[, (nv + 1):(2 * nv)] <- X - Xmax
  
  # g1 and h1 functions
  g1 <- function(X){
    
    write(X,file = paste(getwd(), "evaluate/pop_vars_eval.txt", sep="/"), ncolumns = n_variables, sep = "\t")
    system(paste(paste(getwd(), "moon_mop", sep = "/"), paste(getwd(), "evaluate/", sep = "/"), sep = " "), ignore.stdout = TRUE)
    constraints <- scan(paste(getwd(), "evaluate/pop_cons_eval.txt", sep = "/"), quiet = TRUE)
    constraints <- matrix(constraints, ncol = n_constraints, byrow = TRUE)
    return(constraints)
  }
  
  # Calculate g1(x)
  Cmatrix[, (2*nv + 1):(2*nv + n_constraints)] <- -g1(X)
  
  # Assemble matrix of *violations*
  Vmatrix <- Cmatrix
  Vmatrix[, 1:(2 * nv + n_constraints)] <- pmax(Vmatrix[, 1:(2 * nv + n_constraints)], 0)        # inequality constraints
  
  v = rowSums(Vmatrix)  
  if(is.null(parent.frame(2)$iter)){
    v[which(v != 0)] = (v[which(v != 0)] - min(v))/(max(v) - min(v)) + 0.000001
  }
  else{
    e = parent.frame(2)
    Vtmatrix = e$Vt$Vmatrix
    vt = rowSums(Vtmatrix)
    e$Vt$v[which(vt != 0)] = (vt[which(vt != 0)] - min(v,vt))/(max(v,vt) - min(v,vt)) + 0.000001
    v[which(v != 0)] = (v[which(v != 0)] - min(v,vt))/(max(v,vt) - min(v,vt)) + 0.000001
  }
  # Return necessary variables
  return(list(Cmatrix = Cmatrix,
              Vmatrix = Vmatrix,
              v       = v))
}