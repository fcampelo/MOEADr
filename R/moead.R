#' MOEA/D
#'
#' MOEA/D implementation in R
#'
#' MOEA/D implementation in R
#'
#' @section Problem Description:
#' The \code{probpars} parameter consists of a list with all necessary
#' definitions for the multiobjective optimization problem to be solved.
#' \code{probpars} must contain at least the following fields:
#' \itemize{
#'    \item \code{$name} - name of the problem instance function, that is, a
#'          routine that calculates y = f(x);
#'    \item \code{$xmin} - vector of lower bounds of each variable
#'    \item \code{$xmax} - vector of upper bounds of each variable
#'    \item \code{$nobj} - integer containing the number of objectives
#' }
#'
#' The function indicated in \code{probpars$name} must be able to receive a
#' matrix with each row representing one candidate solution, and return a matrix
#' with each row representing the objective values for that solution.
#'
#' @section Decomposition Methods:
#' The \code{decopars} parameter defines the method to be used for the
#' generation of the decomposition vectors. \code{decopars} must have
#' at least the \code{$name} parameter. Currently implemented methods are:
#'
#' \itemize{
#'    \item \code{$name = "Das"}      - Das and Dennis (1998);
#'    \item \code{$name = "Li"}       - Li et al. (2014);
#'    \item \code{$name = "Uniform"}  - Uniformly distributed vectors;
#' }
#'
#' Check \code{\link{decompose_problem}} for details.
#'
#' @section Scalarization Methods:
#' The \code{scalpars} parameter defines the method to be used for the
#' scalarization of the multiobjective function. \code{scalpars} must have
#' at least the \code{$name} parameter. Currently implemented methods are:
#' \itemize{
#'    \item \code{$name = "ws"}  - Weighted Sum;
#'    \item \code{$name = "wt"}  - Weighted Tchebycheff;
#'    \item \code{$name = "pbi"} - Penalty-based Boundary Intersection;
#'    \item \code{$name = "mwt"} - Modified Weighted Tchebycheff;
#' }
#'
#' See \code{\link{scalarize_values}} for details.
#'
#' @section Variation Operators:
#' TODO: Write a description of the \code{chngpars} structure.
#'
#' @section Stop Criteria:
#' TODO: Write a description of the \code{stopcrit} structure.
#'
#' @section References:
#' Q. Zhang and H. Li, "MOEA/D: A Multiobjective Evolutionary Algorithm
#   Based on Decomposition", IEEE Trans. Evol. Comp. 11(6): 712-731, 2007.
#
#' @param probpars List of problem parameters.
#'    See \code{Problem Description} for details.
#' @param decopars List of decomposition method parameters
#'    See \code{Decomposition methods} for details.
#' @param scalpars List of scalarization method parameters
#'    See \code{Scalarization methods} for details.
#' @param chngpars List of variation operator parameters
#'    See \code{Variation operators} for details.
#' @param stopcrit list of stop criteria parameters. See
#'    \code{Stop criteria} for details.
#'
#' @export
#'
#' @examples
#'
#' # MOEA/D as in Zhang and Li (2007) (sec. V-E, p.721-722)
#' library(mco) # For the test problems
#' probpars <- list(name = "mco_zdt", prob.number = 1, nobj = 2,
#'                  xmin = rep(0, 30), xmax = rep(1, 30))
#' decopars <- list(name = "Das", H = 99, neighbors = 20)
#' scalpars <- list(name = "wt")
#' chngpars <- list(sbx     = list(eta = 20, pc = 1, eps = 1e-16),
#'                  polymut = list(eta = 20, pm = 0.1))
#' updtpars <- list(method = "standard")
#' showpars <- list(show.iters = "numbers", showevery = 1)
#' stopcrit <- list(names = "stop_maxiter",
#'                  maxiter = 500)
#' seed     <- NULL
#' OUT1     <- moead(probpars, decopars, scalpars,
#'                   chngpars, updtpars, showpars,
#'                   stopcrit, seed)
#' with(OUT1, plot(Y[, 1], Y[, 2], pch=20))
#'
#' # MOEA/D as in Zhang and Li (2007) (using PBI, sec. V-F, p. 724)
#' scalpars <- list(name = "pbi", theta = 5)
#' OUT2 <- moead(probpars, decopars, scalpars,
#'               chngpars, updtpars, showpars,
#'               stopcrit, seed)
#' with(OUT2, plot(Y[, 1], Y[, 2], pch=20))
#'
#' # MOEA/D as in Zhang and Li (2007) (with obj normalization, sec. V-F, p. 724)
#' scalpars <- list(name = "wt", normalize.obj = FALSE)
#' probpars <- probpars <- list(name = "mco_zdt", prob.number = 2, nobj = 2,
#'                  xmin = rep(0, 30), xmax = rep(1, 30))
#' chngpars <- list(sbx     = list(eta = 20, pc = 1, eps = 1e-16),
#'                  polymut = list(eta = 20, pm = 1/30))
#' decopars <- list(name = "Das", H = 149, neighbors = 20)
#' OUT3 <- moead(probpars, decopars, scalpars,
#'               chngpars, updtpars, showpars,
#'               stopcrit, seed)
#' with(OUT3, plot(Y[, 1], Y[, 2], pch=20))
#'
#' #' # MOEA/D-DE as in Zhang and Li (2009)
#' scalpars <- list(name = "wt", normalize.obj = FALSE)
#' probpars <- probpars <- list(name = "mco_zdt", prob.number = 1, nobj = 2,
#'                  xmin = rep(0, 30), xmax = rep(1, 30))
#' chngpars <- list(DEops   = list(delta = 0.9, CR = 1, F = 0.5),
#'                  polymut = list(eta = 20, pm = 1/30))
#' decopars <- list(name = "Das", H = 99, neighbors = 20)
#' stopcrit <- list(names = "stop_maxiter",
#'                  maxiter = 200)
#' updtpars <- list(method = "moead-de", nr = 2)
#' OUT4 <- moead(probpars, decopars, scalpars,
#'               chngpars, updtpars, showpars,
#'               stopcrit, seed)
#' with(OUT4, plot(Y[, 1], Y[, 2], pch=20))
#'
moead <- function(probpars,    # Info about the MObj problem
                  decopars,    # Info about the decomposition method
                  scalpars,    # Info about the scalarization method
                  chngpars,    # Info about the variation operators
                  updtpars,    # Info about the update method
                  showpars,    # Info about the function output
                  stopcrit,    # Info about the stop criteria
                  seed = NULL) # Seed for PRNG
{

  # ========== Error catching and default value definitions

  # Check chngpars
  # (The error checking for each specific variation operator is performed by
  # its corresponding function)
  assertthat::assert_that(all(names(chngpars) %in% c("sbx",
                                                     "polymut",
                                                     "DEops")))

  # probpars is checked within routine "create_population()"
  # decopars is checked within routine "decompose_problem()"
  # scalpars is checked within routine "scalarize_values()"
  # updtpars is checked within routine "update_population()"

  # Check seed
  if (is.null(seed)) {
    seed <- as.integer(Sys.time())
  } else {
    assertthat::assert_that(assertthat::is.count(seed))
  }
  # ========== Initial setup
  # Setup PRNG
  set.seed(seed)

  # Generate weigth vectors
  W <- decompose_problem(decopars = decopars,
                         m        = probpars$nobj)

  # Define closest neighbors
  N <- cbind(1:nrow(W),
             FNN::get.knn(data = W,
                          k    = decopars$neighbors - 1)$nn.index)

  # Generate initial population
  X <- create_population(popsize  = nrow(W),
                         probpars = probpars)

  # Evaluate population on objectives
  Y <- evaluate_population(X        = X,
                           probpars = probpars)


  # ========== Iterative cycle
  keep.running  <- TRUE      # stop criteria flag
  iters         <- 0         # counter: iterations
  nfe           <- nrow(X)   # counter: function evaluations

  while(keep.running){
    # Update iteration counter
    iters <- iters + 1

    # Store current population
    Xt <- X
    Yt <- Y

    # Perform variation
    for (opname in names(chngpars)){
      X <- do.call(opname,
                   args = list())
    }

    # Evaluate offspring population on objectives
    Y <- evaluate_population(X        = X,
                             probpars = probpars)

    # Update function evaluations counter
    nfe <- nfe + nrow(X)

    # Update population
    update_population()

    # Echo whatever is demanded
    print_progress()

    # Verify stop criteria
    check_stop_criteria()
  }

  # Output
  return(list(X     = X,
              Y     = Y,
              W     = W,
              nfe   = nfe,
              iters = iters))
}
