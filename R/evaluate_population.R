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
#'
#' WARNING: The calling environment must have a counter variable \code{nfe},
#' which tracks the total number of function evaluations. This routine
#' accesses and updates that \code{nfe} variable.
#'
#' @section Problem Description:
#' The \code{problem} parameter consists of a list with all necessary
#' definitions for the multiobjective optimization problem to be solved.
#' \code{evaluate_population} expects \code{problem} to contain at least the
#' following fields:
#'    - \code{$name} - name of the problem instance function, that is, a routine
#'    that calculates **Y** = **f**(**X**);
#'    - \code{$xmin} - vector of lower bounds of each variable
#'    - \code{$xmax} - vector of upper bounds of each variable
#'    - \code{$m}    - integer containing the number of objectives
#'
#' The function indicated in \code{problem$name} must be able to receive a
#' matrix with each row representing one candidate solution, and return a matrix
#' with each row representing the objective values for one solution. The name of
#' the input argument that receives the population matrix must be either
#' \code{X} or \code{x}.
#'
#' @param X Population matrix of the MOEA/D (each row is a candidate solution).
#' If \code{NULL} the function searches for \code{X} in the calling environment.
#' The candidate solutions must be expressed in standardized form (see
#' \code{Description} for details). When called as part of the [moead()] flow,
#' this is already guaranteed.
#' @param problem list of named problem parameters. If NULL the function
#' searches for \code{problem} in the calling environment.
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

  # REMOVED: there is no guarantee of truncation, so box constraints may be
  # violated from time to time. This has to be dealt with by adding
  # variation_truncate to the variation stack, or be the constraint handling
  # method of choice.
  # assertthat::assert_that(is_within(X, 0, 1, strict = c(FALSE, FALSE)))

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

  if ("vname" %in% names(problem))
  {

    # Prepare arguments for function call
    vfun.args <- as.list(formals(problem$vname))

    my.vargs  <- sapply(names(vfun.args),
                        FUN = function(argname, pars, args){
                          if(argname %in% names(pars)) {
                            args[argname] <- pars[argname]
                          }
                          return(args[[argname]])},
                        pars = problem,
                        args = vfun.args,
                        simplify = FALSE)

    my.vargs[[grep("[x|X]",
                   names(my.vargs))]] <- X

    V <- do.call(problem$vname,
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
