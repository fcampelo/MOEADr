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

evaluate_population_moon <- function(X, problem, nfe, iter, cons = NULL)
{
  # ========== Error catching and default value definitions
  # Input "problem" is assumed to have been already verified in
  # create_population(), and will not be re-checked here.
  assertthat::assert_that(
    is.matrix(X),
    is.numeric(X),
    ncol(X) == length(problem$xmax),
    nfe == as.integer(nfe),
    nfe >= 0,
    iter == as.integer(iter),
    iter >= 0
  )
  
  # ==========
  
  # Denormalize population
  X <- denormalize_population(X, problem)
    filename <-
      paste0("~/MOEADr/th_run/optimizer/interface/pop_vars_eval.txt")
    write.table(
      X,
      file = filename,
      row.names = FALSE,
      sep = "\t",
      col.names = FALSE
    )
    
    #evaluate solutions
    old.wd <- getwd()
    setwd("~/MOEADr/th_run/optimizer/interface/")
    system("./moon_mop .")
    
    # get evaluations from file
    Y <-
      as.matrix(read.csv(
        paste0("pop_objs_eval.txt"),
        sep = "\t",
        stringsAsFactors =  F,
        header = F
      ))
    
    setwd(old.wd)
  
  if ("constraints" %in% names(problem))
  {
    
      # get constrains from file
      old.wd <- getwd()
      setwd("~/MOEADr/th_run/optimizer/interface/")
      filename <- "pop_cons_eval.txt"
      cons <-
        as.matrix(read.csv(
          filename,
          sep = "\t",
          stringsAsFactors =  F,
          header = F
        ))
      # cons <- (-1)*cons
      
      # I dont know why did I do this - no moead.r parece que tem que ser contraints maior que zero
      # Vmatrix <- pmax(cons, 0)
      cons <- scaling_Y(cons, cons)
      Vmatrix <- cons
      
      V <-
        list(Cmatrix = cons,
             Vmatrix = Vmatrix,
             v = rowSums(Vmatrix))
      
      setwd(old.wd)
    
  }
  else
  {
    V <- NULL
  }
  
  # Update evaluations counter in the calling environment
  nfe <- nfe + nrow(X)
  
  R <- list(Y   = Y,
            V   = V,
            nfe = nfe,
            cons = cons)
  return(R)
}