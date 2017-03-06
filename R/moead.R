#' MOEA/D
#'
#' MOEA/D implementation in R
#'
#' Component-wise implementation of the Multiobjective Evolutionary Algorithm
#' based on decomposition - MOEA/D.
#'
#' @section Problem Description:
#' The `problem` parameter consists of a list with all necessary
#' definitions for the multiobjective optimization problem to be solved.
#' `problem` must contain at least the following fields:
#'    - `problem$name`: name of the problem instance function, that is, a
#'    routine that calculates **Y** = **f**(**X**);
#'    - `problem$xmin`: vector of lower bounds of each variable
#'    - `problem$xmax`: vector of upper bounds of each variable
#'    - `problem$m`:  integer indicating the number of objectives
#'
#' Besides these fields, `problem` should contain any other relevant inputs
#' for the routine listed in `$name`. `problem` may also contain the
#' (optional) field `problem$constraints`, which is a list object
#' containing information about the problem constraints. If present, this list
#' must have the following fields:
#'    - `problem$constraints$name` - (required) name of the function that
#'        calculates the constraint values (see below for details)
#'    - `problem$constraints$epsilon` - (optional) a small non-negative value
#'        indicating the tolerance to be considered for equality constraints.
#'        Defaults to zero.
#'
#' Besides these fields, `problem$constraint` should contain any other
#' relevant inputs for the routine listed in `problem$constraint$name`.
#'
#' Detailed instructions for defining the routines for calculating the
#' objective and constraint functions are provided in the vignette
#' _Defining Problems in the MOEADr Package_. Check that documentation for
#' details.
#'
#' @section Decomposition Methods:
#' The `decomp` parameter is a list that defines the method to be used for the
#' generation of the weight vectors. `decomp` must have
#' at least the `$name` parameter. Currently available methods can be
#' verified using [get_decomposition_methods()]. Check
#' [generate_weights()] and the information provided by
#' [get_decomposition_methods()] for more details.
#'
#' @section Neighborhood Strategies:
#' The `neighbors` parameter is a list that defines the method for defining the
#' neighborhood relations among subproblems. `neighbors` must have
#' at least three parameters:
#' - `neighbors$name`, name of the strategy used to define the neighborhoods.
#'     Currently available methods are:
#'         - `$name = "lambda"`: uses the distances between weight vectors.
#'            The calculation is performed only once for the entire run,
#'            since the weight vectors are assumed static.
#'         - `$name = "x"`: uses the distances between the incumbent solutions
#'            associated with each subproblem. In this case the calculation is
#'            performed at each iteration, since incumbent solutions may change.
#' - `neighbors$T`: defines the neighborhood size. This parameter must receive
#' a value smaller than the number of subproblems defined for the MOEA/D.
#' - `neighbors$delta.p`: parameter that defines the probability of sampling
#' from the neighborhood when performing variation.
#'
#' Check [define_neighborhood()] for more details.
#'
#'
#' @section Variation Operators:
#' The `variation` parameter consists of a list vector, in which each
#' sublist defines a variation operator to be used as part of the variation
#' block. Each sublist must have at least a field `$name`, containing the name
#' of the `i`-th variation operator to be applied. Use
#' [get_variation_operators()] to generate a list of available operators, and
#' consult the vignette `Variation Stack in the MOEADr Package` for more
#' details.
#'
#' @section Scalar Aggregation Functions:
#' The `aggfun` parameter is a list that defines the scalar aggregation function
#' to be used. `aggfun` must have at least the `$name` parameter. Currently
#' available methods can be verified using [get_scalarization_methods()]. Check
#' [scalarize_values()] and the information provided by
#' [get_scalarization_methods()] for more details.
#'
#' @section Update Methods:
#' The `update` parameter is a list that defines the population update strategy
#' to be used. `update` must have at least the `$name` parameter. Currently
#' available methods can be verified using [get_update_methods()]. Check
#' [update_population()] and the information provided by
#' [get_update_methods()] for more details.
#'
#' Another (optional) field of the `update` parameter is `update$UseArchive`,
#' which is a binary flag defining whether the algorithm should keep an
#' external solution archive (`TRUE`) or not (`FALSE`). Since it adds to the
#' computational burden and memory requirements of the algorithm, the use of an
#' archive population is recommended only in the case of constrained problems
#' with constraint handling method that can occasionaly accept unfeasible
#' solutions, leading to the potential loss of feasible efficient solutions for
#' certain subproblems (e.g., [constraint_vbr()] with `type` = "sr" or "vt").
#'
#' @section Constraint Handling Methods:
#' The `constraint` parameter is a list that defines the constraint-handling
#' technique to be used. `constraint` must have at least the `$name` parameter.
#' Currently available methods can be verified using [get_constraint_methods()].
#' Check [update_population()] and the information provided by
#' [get_constraint_methods()] for more details.
#'
#' @section Objective Scaling:
#' Objective scaling refers to the re-scaling of the objective values at each
#' iteration, which is generally considered to prevent problems arising from
#' differently-scaled objective functions. `scaling` is a list that must have
#' at least the `$name` parameter. Currently available options are
#' `$name = "none"`, which does not perform any scaling, and `$name = "simple"`,
#' which performs a simple linear scaling of the objectives to the interval
#' `[0, 1]`.
#'
#' @section Stop Criteria:
#' The `stopcrit` parameter consists of a list vector, in which each
#' sublist defines a termination criterion to be used for the MOEA/D. Each
#' sublist must have at least a field `$name`, containing the name of the
#' `i`-th criterion to be verified. The iterative cycle of the MOEA/D is
#' terminated whenever any criterion is met. Use [get_stop_criteria()] to
#' generate a list of available criteria, and check the information provided by
#' that function for more details.
#'
#' @section Echoing Options:
#' The `showpars` parameter is a list that defines the echoing options of the
#' MOEA/D. `showpars` must contain two fields:
#' - `showpars$show.iters`, defining the type of echoing output. `$show.iters`
#' can be set as `"none"`, `"numbers"`, or `"dots"`.
#' - `showpars$showevery`, defining the period of echoing (in iterations).
#' `$showevery` must be a positive integer.
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
#'    See [print_progress()] for details.
#' @param seed seed for the pseudorandom number generator. Defaults to NULL,
#'    in which case \code{as.integer(Sys.time())} is used for the definition.
#'
#' @export
#'
#' @return List object containing :
#'
#' - information on the final population (`X`), its objective values (`Y`) and
#'  constraint information list (`V`) (see [evaluate_population()] for details);
#' - Archive population list containing its corresponding `X`, `Y` and `V`
#'  fields (only if `update$UseArchive = TRUE`).
#' - Estimates of the _ideal_ and _nadir_ points, calculated for the final
#' population;
#' - Number of function evaluations, iterations, and total execution time;
#' - Random seed employed in the run, for reproducibility
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
#' update    <- list(name       = "standard", UseArchive = FALSE)
#' scaling   <- list(name       = "none")
#' constraint<- list(name       = "none")
#' stopcrit  <- list(list(name  = "maxiter",
#'                     maxiter  = 200))
#' showpars  <- list(show.iters = "dots",
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
#' # Rerun with MOEA/D-DE configuration and AWT scalarization
#' aggfun    <- list(name       = "awt")
#' variation <- list(list(name  = "diffmut",
#'                        basis = "rand",
#'                        Phi   = NULL),
#'                   list(name  = "binrec",
#'                        rho   = 0.8),
#'                   list(name = "polymut",
#'                        etam  = 20, pm = 0.1),
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

  # ============== Error catching and default value definitions ============== #
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
  # ============ End Error catching and default value definitions ============ #

  # ============================= Algorithm setup ============================ #
  set.seed(seed)               # set PRNG seed
  nfe        <- 0              # counter for function evaluations
  time.start <- Sys.time()     # Store initial time
  iter.times <- numeric(10000) # pre-allocate vector for iteration times.
  if(is.null(update$UseArchive)){
    update$UseArchive <- FALSE
  }
  # =========================== End Algorithm setup ========================== #

  # =========================== Initial definitions ========================== #
  # Generate weigth vectors
  W  <- generate_weights(decomp = decomp,
                         m      = problem$m)

  # Generate initial population
  X  <- create_population(N       = nrow(W),
                          problem = problem)

  # Evaluate population on objectives
  YV <- evaluate_population(X       = X,
                            problem = problem,
                            nfe     = nfe)
  Y   <- YV$Y
  V   <- YV$V
  nfe <- YV$nfe

  # ========================= End Initial definitions ======================== #

  # ============================= Iterative cycle ============================ #
  keep.running  <- TRUE      # stop criteria flag
  iter          <- 0         # counter: iterations

  while(keep.running){
    # Update iteration counter
    iter <- iter + 1

    # ========== Neighborhoods
    # Define/update neighborhood probability matrix
    BP <- define_neighborhood(neighbors = neighbors,
                              v.matrix  = switch(neighbors$name,
                                                 lambda = W,
                                                 x      = X),
                              iter      = iter)
    B  <- BP$B
    P  <- BP$P

    # ========== Variation
    # Store current population
    Xt <- X
    Yt <- Y
    Vt <- V

    # Perform variation
    Xv      <- do.call(perform_variation,
                       args = as.list(environment()))
    X       <- Xv$X
    ls.args <- Xv$ls.args
    nfe     <- nfe + Xv$var.nfe

    # ========== Evaluation
    # Evaluate offspring population on objectives
    YV <- evaluate_population(X       = X,
                              problem = problem,
                              nfe     = nfe)
    Y   <- YV$Y
    V   <- YV$V
    nfe <- YV$nfe

    # ========== Scalarization
    # Objective scaling and estimation of 'ideal' and 'nadir' points
    normYs <- scale_objectives(Y       = Y,
                               Yt      = Yt,
                               scaling = scaling)

    # Scalarization by neighborhood.
    # bigZ is an [(T+1) x N] matrix, in which each column has the T scalarized
    # values for the solutions in the neighborhood of one subproblem, plus the
    # scalarized value for the incumbent solution for that subproblem.
    bigZ <- scalarize_values(normYs  = normYs,
                             W       = W,
                             B       = B,
                             aggfun  = aggfun)

    # Calculate selection indices
    # sel.indx is an [N x (T+1)] matrix, in which each row contains the indices
    # of one neighborhood (plus incumbent), sorted by their "selection quality"
    # (which takes into account both the performance value and constraint
    # handling policy, if any)
    sel.indx <- order_neighborhood(bigZ       = bigZ,
                                   B          = B,
                                   V          = V,
                                   Vt         = Vt,
                                   constraint = constraint)

    # ========== Update
    # Update population
    XY <- do.call(update_population,
                  args = as.list(environment()))
    X       <- XY$X
    Y       <- XY$Y
    V       <- XY$V
    Archive <- XY$Archive

    # ========== Stop Criteria
    # Calculate iteration time
    elapsed.time <- as.numeric(difftime(time1 = Sys.time(),
                                        time2 = time.start,
                                        units = "secs"))
    iter.times[iter] <- ifelse(iter == 1,
                               yes = as.numeric(elapsed.time),
                               no  = as.numeric(elapsed.time) - sum(iter.times))

    # Verify stop criteria
    keep.running <- check_stop_criteria(stopcrit = stopcrit,
                                        call.env = environment())

    # ========== Print
    # Echo whatever is demanded
    print_progress(iter.times, showpars)
  }
  # =========================== End Iterative cycle ========================== #

  # ================================== Output ================================ #
  # Prepare output
  X <- denormalize_population(X, problem)

  if(!is.null(Archive)) Archive$X <- denormalize_population(Archive$X, problem)

  # Output
  return(list(X       = X,
              Y       = Y,
              V       = V,
              W       = W,
              Archive = Archive,
              ideal   = apply(Y, 2, min),
              nadir   = apply(Y, 2, max),
              nfe     = nfe,
              n.iter  = iter,
              time    = difftime(Sys.time(), time.start, units = "secs"),
              seed    = seed))
  # ================================ End Output ============================== #
}

