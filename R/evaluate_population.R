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
#' **Warning**: The calling environment must have a counter variable \code{nfe},
#' which tracks the total number of function evaluations. This routine
#' accesses and updates that \code{nfe} variable.
#'
#'
#' @param X Population matrix of the MOEA/D (each row is a candidate solution).
#' If \code{NULL} the function searches for \code{X} in the calling environment.
#' @param problem list of named problem parameters. If NULL the function
#' searches for \code{problem} in the calling environment. See Section
#' \code{Problem Description} of the [moead()] documentation for details.
#'
#' @return [N x m] matrix of objective function values, where \code{N} is the
#' population size and \code{m} is the number of objectives.
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

  if ("constraints" %in% names(problem))
  {
    con <- problem$constraints

    # Prepare arguments for function call
    vfun.args <- as.list(formals(con$name))

    my.vargs  <- sapply(names(vfun.args),
                        FUN = function(argname, pars, args){
                          if(argname %in% names(pars)) {
                            args[argname] <- pars[argname]
                          }
                          return(args[[argname]])},
                        pars = con,
                        args = vfun.args,
                        simplify = FALSE)

    my.vargs[[grep("[x|X]",
                   names(my.vargs))]] <- X

    V <- do.call(con$name,
                 args = my.vargs)
  }
  else
  {
    V <- NULL
  }

  call.env$nfe <- call.env$nfe + nrow(X)

  R <- list(Y = Y, V = V)
  return(R)
}
