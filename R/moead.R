#' MOEA/D
#'
#' MOEA/D implementation in R
#'
#' Component-wise implementation of the Multiobjective Evolutionary Algorithm
#' based on decomposition - MOEA/D.
#'
#' @section Problem Description:
#' The \code{problem} parameter consists of a list with all necessary
#' definitions for the multiobjective optimization problem to be solved.
#' \code{problem} must contain at least the following fields:
#'    - \code{$name} - name of the problem instance function, that is, a routine
#'    that calculates **Y** = **f**(**X**);
#'    - \code{$xmin} - vector of lower bounds of each variable
#'    - \code{$xmax} - vector of upper bounds of each variable
#'    - \code{$m}    - integer containing the number of objectives
#'
#' Besides these fields, \code{problem} should contain any other relevant inputs
#' for the routine listed in \code{$name}. \code{problem} may also contain the
#' (optional) field \code{problem$constraints}, which is a list object
#' containing information about the problem constraints. If present, this list
#' must have the following fields:
#'    - \code{$constraints$name} - (required) name of the function that
#'        calculates the constraint values (see below for details)
#'    - \code{$constraints$epsilon} - (optional) a small non-negative value
#'        indicating the tolerance to be considered for equality constraints.
#'        Defaults to zero.
#'
#' Besides these fields, \code{problem$constraint} should contain any other
#' relevant inputs for the routine listed in \code{problem$constraint$name}.
#'
#' Detailed instructions for defining the routines for calculating the
#' objective and constraint functions are provided in the vignette
#' _Defining Problems in the MOEADr Package_. Check that documentation for
#' details.
#'
#' @section Decomposition Methods:
#' The \code{decomp} parameter defines the method to be used for the
#' generation of the weight vectors. \code{decomp} must have
#' at least the \code{$name} parameter. Currently implemented methods can be
#' verified using \code{get_decomposition_methods()}. Check
#' \code{\link{generate_weights}} and the information provided by
#' \code{get_decomposition_methods()} for more details.
#'
#' @section Neighborhood Strategies:
#' The \code{neighbors} parameter defines the method for defining the
#' neighborhood relations among subproblems. \code{neighbors} must have
#' at least three parameters: \code{neighbors$name}, \code{neighbors$T}, and
#' \code{neighbors$delta.p}.
#' \code{neighbors$name} defines the strategy used to define the neighborhoods.
#' Currently available methods are:
#' \itemize{
#'    \item \code{$name = "lambda"}: uses the distances between
#'    weight vectors. The calculation is performed only once for the entire run,
#'    since the weight vectors are assumed static.
#'    \item \code{$name = "x"}: uses the distances between the
#'    incumbent solutions associated with each subproblem. In this case the
#'    calculation is performed at each iteration, since incumbent solutions may
#'    change.
#' }
#'
#' \code{neighbors$T} defines the neighborhood size. This parameter must receive
#' a value smaller than the number of subproblems defined for the MOEA/D.
#'
#' Finally, \code{neightbors$delta.p} is the parameter that defines the
#' probability of sampling from the neighborhood when performing variation.
#'
#' Check \code{\link{define_neighborhood}} for more details.
#'
#'
#' @section Variation Operators:
#' TODO
#'
#' @section Scalar Aggregation Functions:
#' TODO
#'
#' @section Update Methods:
#' TODO
#'
#' @section Constraint Handling Methods:
#' TODO
#'
#' @section Objective Scaling:
#' TODO
#'
#' @section Stop Criteria:
#' TODO
#'
#' @section Echoing Options:
#' TODO
#'
#' @section References:
#' F. Campelo, L.S. Batista, C. Aranha:
#' "A Component-Wise Perspective on Multiobjective Evolutionary Algorithms
#' based on Decomposition". In preparation, 2016.
#
#' @param problem List containing the problem parameters.
#'    See \code{Problem Description} for details.
#' @param decomp List containing the decomposition method parameters
#'    See \code{Decomposition methods} for details.
#' @param aggfun List containing the aggregation function parameters
#'    See \code{Scalarization methods} for details.
#' @param neighbors List containing the decomposition method parameters
#'    See \code{Neighborhood strategies} for details.
#' @param variation List containing the variation operator parameters
#'    See \code{Variation operators} for details.
#' @param update List containing the population update parameters
#'    See \code{Update strategies} for details.
#' @param constraint List containing the constraint handing parameters
#'    See \code{Constraint operators} for details.
#' @param scaling List containing the objective scaling parameters
#'    See \code{Objective scaling} for details.
#' @param stopcrit list containing the stop criteria parameters.
#'    See \code{Stop criteria} for details.
#' @param showpars list containing the echoing behavior parameters.
#'    Use \code{?print_progress} for details.
#' @param seed seed for the pseudorandom number generator. Defaults to NULL,
#'    in which case \code{as.integer(Sys.time())} is used for the definition.
#'
#' @export
#'
#' @examples
#'
#' # MOEA/D as in Zhang and Li (2007) (sec. V-E, p.721-722)
#' ## 1: prepare test problem
#' library(smoof)
#' ZDT1 <- make_vectorized_smoof(prob.name  = "ZDT1",
#'                               dimensions = 30)
#'
#' ## 2: set input parameters
#' problem   <- list(name       = "ZDT1",
#'                   xmin       = rep(0, 30),
#'                   xmax       = rep(1, 30),
#'                   m          = 2)
#' decomp    <- list(name       = "SLD", H = 99)
#' neighbors <- list(name       = "lambda",
#'                   T          = 20,
#'                   delta.p    = 1)
#' aggfun    <- list(name       = "wt")
#' variation <- list(list(name  = "sbx",
#'                        etax  = 20, pc = 1),
#'                   list(name  = "polymut",
#'                        etam  = 20, pm = 0.1),
#'                   list(name  = "truncate"))
#' update    <- list(name       = "standard")
#' scaling   <- list(name       = "simple")
#' constraint<- list(name       = "none")
#' stopcrit  <- list(list(name  = "maxiter",
#'                     maxiter  = 200))
#' showpars  <- list(show.iters = "numbers",
#'                   showevery  = 10)
#' seed      <- NULL
#'
#' ## 3: run MOEA/D
#' out1 <- moead(problem, decomp,  aggfun, neighbors, variation, update,
#'               constraint, scaling, stopcrit, showpars, seed)
#'
#' # 4: Plot output:
#' plot(out1$Y[,1], out1$Y[,2], type = "p", pch = 20)
#'
#'
#' # Rerun with standard DE variation operators (rand mutation + binomial
#' # recombination)
#' variation <- list(list(name  = "diffmut",
#'                        basis = "rand",
#'                        Phi   = NULL),
#'                   list(name  = "binrec",
#'                        rho   = 0.8),
#'                   list(name  = "truncate"))
#'
#' out2 <- moead(problem, decomp,  aggfun, neighbors, variation, update,
#'               constraint, scaling, stopcrit, showpars, seed)
#' plot(out2$Y[,1], out2$Y[,2], type = "p", pch = 20)

moead <- function(problem,      # List:  MObj problem
                  decomp,       # List:  decomposition strategy
                  aggfun,       # List:  scalar aggregation function
                  neighbors,    # List:  neighborhood assignment strategy
                  variation,    # List:  variation operators
                  update,       # List:  update method
                  constraint,   # List:  constraint handling method
                  scaling,      # List:  objective scaling strategy
                  stopcrit,     # List:  stop criteria
                  showpars,     # List:  echoing behavior
                  seed = NULL)  # Seed for PRNG
{

  # ========== Error catching and default value definitions
  # "problem"     checked in "create_population(...)"
  # "decomp"      checked in "decompose_problem(...)"
  # "aggfun"      checked in "scalarize_values(...)"
  # "neighbors"   checked in "define_neighborhood(...)"
  # "variation"   checked in "perform_variation(...)"
  # "update"      checked in "update_population(...)"
  # "scaling"     checked in
  # "repair"      checked in
  # "stopcrit"    checked in
  # "showpars"    checked in

  # Check seed
  if (is.null(seed)) {
    seed <- as.integer(Sys.time())
  } else {
    assertthat::assert_that(assertthat::is.count(seed))
  }
  # ==========

  # ========== Algorithm setup
  set.seed(seed)  # set PRNG seed
  nfe <- 0        # set counter for function evaluations
  time.start <- Sys.time() # Store initial time

  # ========== Initial definitions
  # Generate weigth vectors
  W  <- generate_weights(m = problem$m)

  # Generate initial population
  X  <- create_population(nrow(W))

  # Evaluate population on objectives
  YV <- evaluate_population(X)
  Y  <- YV$Y
  V  <- YV$V
  # ==========

  # ========== Iterative cycle
  keep.running  <- TRUE      # stop criteria flag
  iter          <- 0         # counter: iterations

  while(keep.running){
    # Update iteration counter
    iter <- iter + 1

    # Define/update neighborhood probability matrix
    BP <- define_neighborhood()
    B  <- BP$B
    P  <- BP$P

    # Store current population
    Xt <- X
    Yt <- Y
    Vt <- V

    # Perform variation
    X <- perform_variation(X, P, B, W, variation)

    # Evaluate offspring population on objectives
    YV <- evaluate_population(X)
    Y <- YV$Y
    V <- YV$V

    # Update population
     update_population()

    # Echo whatever is demanded
    print_progress()

    # Verify stop criteria
    check_stop_criteria()
  }
  # ==========

  # ========== Output
  # Prepare output
  X <- denormalize_population(X, problem)

  # Output
  return(list(X      = X,
              Y      = Y,
              V      = V,
              W      = W,
              ideal  = apply(Y, 2, min),
              nadir  = apply(Y, 2, max),
              nfe    = nfe,
              n.iter = iter,
              time   = difftime(Sys.time(), time.start),
              seed   = seed))
}

