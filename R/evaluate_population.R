#' Evaluate population
#'
#' Evaluate a population matrix on the objective functions for the MOEADr
#' package
#'
#' This routine evaluates a population matrix for the MOEA/D. Each row of the
#' matrix is considered as a candidate solution. This routine expects the
#' candidate solutions to be standardized, i.e., that the variable limits given
#' in \code{problem$xmin} and \code{problem$xmax} are mapped to \code{0} and
#' \code{1}, respectively.
#'
#' @param X Population matrix of the MOEA/D (each row is a candidate solution).
#' @param problem list of named problem parameters. See Section
#' `Problem Description` of the [moead()] documentation for details.
#' @param nfe counter of function evaluations from the [moead()] routine.
#'
#' @return List object containing the matrix of objective function values,
#' a list object containing information about the constraint violations (a
#' matrix of constraint values `Cmatrix`, a matrix of constraint violations
#' `Vmatrix`, and a vector of total violations `v`), and the updated counter
#' `nfe`.
#'
#' @examples
#' ex.problem <- list(name = "example_problem",
#'                    xmin = rep(-1, 5),
#'                    xmax = rep(1, 5),
#'                    m    = 2)
#' X <- create_population(20, ex.problem)
#' Y <- evaluate_population(X, ex.problem, nfe = 0)
#'
#' @export

evaluate_population <- function(X, problem, nfe, iter, my.file.n)
{

  # ========== Error catching and default value definitions
  # Input "problem" is assumed to have been already verified in
  # create_population(), and will not be re-checked here.
  assertthat::assert_that(is.matrix(X),
                          is.numeric(X),
                          ncol(X) == length(problem$xmax),
                          nfe == as.integer(nfe),
                          nfe >= 0)

  # ==========

  # Denormalize population
  if (problem$name == "problem.moon") {
    
    
    filename <- paste0(my.file.n, "th_run/optimizer/interface/pop_vars_eval.txt")
    write.table(X,
                file = filename,
                row.names = FALSE, sep = "\t",  col.names=FALSE)
    path <- paste0(my.file.n, "th_run/optimizer/interface/")
    system(paste("./moon_mop",path))
    # get evaluations from file
    Y <- as.matrix(read.csv(paste0(my.file.n, "th_run/optimizer/interface/pop_objs_eval.txt"), sep = "\t", stringsAsFactors =  F, header = F))
    my.iter <- with_options(
      c(scipen = 999), 
      str_pad(iter, 4, pad = "0")
    )
    dest <- paste0(my.file.n, "th_run/optimizer/interface/gen",my.iter,"_pop_vars_eval.txt")
    system(paste("mv ", filename, dest))
    
    filename <- paste0(my.file.n, "th_run/optimizer/interface/pop_objs_eval.txt")
    dest <- paste0(my.file.n, "th_run/optimizer/interface/gen",my.iter,"_pop_objs_eval.txt")
    system(paste("mv ", filename, dest))
  }
  else{
    X <- denormalize_population(X, problem)
    
    # Prepare arguments for function call
    fun.args <- as.list(formals(problem$name))
    
    my.args  <- sapply(names(fun.args),
                       FUN      = function(argname, pars, args){
                         if(argname %in% names(pars)) {
                           args[argname] <- pars[argname]
                         }
                         return(args[[argname]])},
                       pars     = problem,
                       args     = fun.args,
                       simplify = FALSE)
    
    my.args[[grep("[x|X]",
                  names(my.args))]] <- X
    
    Y <- do.call(problem$name,
                 args = my.args)
  }
  if ("constraints" %in% names(problem))
  {
    if(problem$name == "problem.moon") {
      # get constrains from file
      cons <- as.matrix(read.csv("pop_cons_eval.txt", sep = "\t", stringsAsFactors =  F, header = F))
      Vmatrix <- abs(pmin(cons[, 1:2], 0))
      V <- list(Cmatrix = cons, Vmatrix = Vmatrix, v = rowSums(Vmatrix))
      filename <- paste0(my.file.n, "th_run/optimizer/interface/pop_cons_eval.txt")
      dest <- paste0(my.file.n, "th_run/optimizer/interface/gen",my.iter,"_pop_cons_eval.txt")
      system(paste("mv ", filename, dest))
    }
    else{
      con <- problem$constraints
      if (is.null(con$epsilon)) con$epsilon <- 0
      
      # Prepare arguments for function call
      vfun.args <- as.list(formals(con$name))
      
      my.vargs  <- sapply(names(vfun.args),
                          FUN      = function(argname, pars, args){
                            if(argname %in% names(pars)) {
                              args[argname] <- pars[argname]
                            }
                            return(args[[argname]])},
                          pars     = con,
                          args     = vfun.args,
                          simplify = FALSE)
      
      my.vargs[[grep("[x|X]",
                     names(my.vargs))]] <- X
      
      V <- do.call(con$name,
                   args = my.vargs)
    }
    
  }
  else
  {
    V <- NULL
  }

  # Update evaluations counter in the calling environment
  nfe <- nfe + nrow(X)

  R <- list(Y   = Y,
            V   = V,
            nfe = nfe)
  return(R)
}
