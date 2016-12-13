#' Evaluate population
#'
#' Evaluate a population matrix on the objective functions for the MOEADr
#' package
#'
#' This routine evaluates a population matrix for the MOEA/D. Each row of the
#' matrix is considered as a candidate solution. This routine expects the
#' candidate solutions to be expressed in the [0,1] interval.
#'
#' WARNING: The calling environment must have a counter variable \code{nfe},
#' which tracks the total number of function evaluations. This routine
#' accesses and updates that \code{nfe} variable.
#'
#' @section Problem Description:
#' The \code{problem} parameter consists of a list with all necessary
#' definitions for the multiobjective optimization problem to be solved.
#' \code{problem} must contain at least the following fields:
#' \itemize{
#'    \item \code{$name} - name of the problem instance function, that is, a
#'          routine that calculates Y = f(X);
#'    \item \code{$xmin} - vector of lower bounds of each variable
#'    \item \code{$xmax} - vector of upper bounds of each variable
#'    \item \code{$m}    - integer containing the number of objectives
#' }
#'
#' The function indicated in \code{problem$name} must be able to receive a
#' matrix with each row representing one candidate solution, and return a matrix
#' with each row representing the objective values for that solution. The name
#' of the input argument that receives the population matrix must be either
#' \code{X} or \code{x}.
#'
#' @param X Population matrix of the MOEA/D (each row is a candidate solution).
#' If \code{NULL} the function searches for \code{X} in the calling environment.
#' @param problem list of named problem parameters. If NULL the function
#' searches for \code{problem} in the calling environment.
#'
#' @return [N x m] matrix of objective function values.
#'
#' @export

evaluate_population <- function(X       = NULL,
                                problem = NULL)
{

  # Capture calling environment
  call.env <- parent.frame()

  # Capture "problem" from calling environment if needed
  if (is.null(problem)) {
    assertthat::assert_that(assertthat::has_name(call.env, "problem"))
    problem <- call.env$problem
  }

  # Capture "X" from calling environment if needed
  if (is.null(X)) {
    assertthat::assert_that(assertthat::has_name(call.env, "X"))
    X <- call.env$X
  }

  # ========== Error catching and default value definitions
  # Input "problem" is assumed to have been already verified in
  # create_population(), and will not be re-checked here.

  assertthat::assert_that(is_within(X, 0, 1, strict = FALSE))

  # ==========

  # Denormalize population
  X <- denormalize_population(X, problem)

  # Prepare arguments for function call
  fun.args <- as.list(formals(problem$name))

  my.args  <- sapply(names(fun.args),
                     FUN = function(argname, pars, args){
                       if(argname %in% names(pars)) {
                         args[argname] <- pars[argname]
                       }
                       return(args[[argname]])},
                     pars = problem,
                     args = fun.args,
                     simplify = FALSE)

  my.args[[grep("[x|X]",
                names(my.args))]] <- X

  Y <- do.call(problem$name,
               args = my.args)

  call.env$nfe <- call.env$nfe + nrow(X)

  return(Y)
}
