# Function responsible to convert the continuous values to the nearest discrete values
Discretize <- function(X){
  nearest = apply(abs(as.matrix(discrete) - X), 1, FUN=which.min)
  k = 0
  discreteValues = c(0)
  for (i in nearest) {
    k = k+1
    discreteValues[k] = discrete[k,i]
  }
  return(discreteValues)
}

# Objective Function for the number of commom parts
EvaluateCommonParts <- function(X){

  X = Discretize(X)
  write(X,file = paste(getwd(), "evaluate/pop_vars_eval.txt", sep="/"), ncolumns = n_variables, sep = "\t")
  system(paste(paste(getwd(), "assets/mazda_mop", sep = "/"), paste(getwd(), "evaluate/", sep = "/"), sep = " "), ignore.stdout = TRUE)
  objectives <- scan(paste(getwd(), "evaluate/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = 5, byrow = TRUE)

  common = matrix(objectives[,2], ncol = 1)
  common
}

# Objecive Function for the total weight
EvaluateWeight <- function(X){

  objectives <- scan(paste(getwd(), "evaluate/pop_objs_eval.txt", sep = "/"), quiet = TRUE)
  objectives <- matrix(objectives, ncol = 5, byrow = TRUE)

  weight = matrix(objectives[,1], ncol = 1)
  weight
}

# Definition of the problem
problem.car <- function(X) {
  t(apply(X, MARGIN = 1,
          FUN = function(X) { c(EvaluateCommonParts(X), EvaluateWeight(X)) }
  ))
}

my_constraints <- function(X)
{
  X = round(X, digits = 1)
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

    X = apply(X, FUN = Discretize,1)
    write(X,file = paste(getwd(), "evaluate/pop_vars_eval.txt", sep="/"), ncolumns = n_variables, sep = "\t")
    system(paste(paste(getwd(), "assets/mazda_mop", sep = "/"), paste(getwd(), "evaluate/", sep = "/"), sep = " "), ignore.stdout = TRUE)
    constraints <- scan(paste(getwd(), "evaluate/pop_cons_eval.txt", sep = "/"), quiet = TRUE)
    constraints <- matrix(constraints, ncol = n_constraints, byrow = TRUE)
    return(constraints)
  }

  # Calculate g1(x)
  Cmatrix[, (2*nv + 1):(2*nv + n_constraints)] <- -g1(X)

  # Assemble matrix of *violations*
  Vmatrix <- Cmatrix
  Vmatrix[, 1:(2 * nv + n_constraints)] <- pmax(Vmatrix[, 1:(2 * nv + n_constraints)], 0)

  v = rowSums(Vmatrix)

  # Scaling the Penalties
  if(is.null(parent.frame(2)$iter)){
    #First generation (No incubent solutions)
    v[which(v != 0)] = (v[which(v != 0)] - min(v))/(max(v) - min(v) + 1e-16) + 0.000001
  }
  else{

    # Getting the incubent solutions
    e = parent.frame(2)
    Vtmatrix = e$Vt$Vmatrix
    vt = rowSums(Vtmatrix)

    # Extract max and min for scaling
    max = max(v,vt)
    min = min(v,vt)

    # Updating the new scaled penalties of the incubent solutions
    e$Vt$v[which(vt != 0)] = (vt[which(vt != 0)] - min)/(max - min + 1e-16) + 0.000001

    # Scaling the new solution's penalties
    v[which(v != 0)] = (v[which(v != 0)] - min)/(max - min + 1e-16) + 0.000001
  }

  # Return necessary variables
  return(list(Cmatrix = Cmatrix,
              Vmatrix = Vmatrix,
              v       = v))
}
