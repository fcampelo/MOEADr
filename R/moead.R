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
#' with constraint handling method that can occasionally accept ugasible
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
#' "The MOEADr Package - A Component-Based Framework for Multiobjective
#' Evolutionary Algorithms Based on Decomposition". In preparation, 2017.
#'
#' @param preset List object containing preset values for one or more
#'    of the other parameters of the `moead` function. Values provided in
#'    the `preset` list will override any other value provided. Presets should be
#'    generated by the [preset_moead()] function.
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
#' @param ... Other parameters (useful for development and debugging, not
#' necessary in regular use)
#'
#' @export
#'
#' @return List object of class _moead_ containing:
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
#' ## Prepare a test problem composed of minimization of the (shifted)
#' ## sphere and Rastrigin functions
#' sphere     <- function(x){sum((x + seq_along(x) * 0.1) ^ 2)}
#' rastringin <- function(x){
#'                 x.shift <- x - seq_along(x) * 0.1
#'                 sum((x.shift) ^ 2 - 10 * cos(2 * Pi * x.shift) + 10)}
#' problem.sr <- function(X){
#'                 t(apply(X, MARGIN = 1,
#'                 FUN = function(X){c(sphere(X), rastringin(X))}))}
#'
#'
#' ## Set the input parameters for the moead() routine
#' ## This reproduces the Original MOEA/D of Zhang and Li (2007)
#' ## (with a few changes in the computational budget, to make it run faster)
#' problem   <- list(name       = "problem.sr",
#'                   xmin       = rep(-1, 30),
#'                   xmax       = rep(1, 30),
#'                   m          = 2)
#' decomp    <- list(name       = "SLD", H = 49) # <-- H = 99 in the original
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
#'                     maxiter  = 50))      # <-- maxiter = 200 in the original
#' showpars  <- list(show.iters = "dots",
#'                   showevery  = 10)
#' seed      <- 42
#'
#' ## Run MOEA/D
#' out1 <- moead(preset = NULL,
#'               problem, decomp, aggfun, neighbors, variation, update,
#'               constraint, scaling, stopcrit, showpars, seed)
#'
#' ## Examine the output:
#' summary(out1)
#'
#' ## Alternatively, the standard MOEA/D could also be set up using the
#' ## preset_moead() function. The code below runs the original MOEA/D with
#' ## exactly the same configurations as in Zhang and Li (2007).
#' \dontrun{
#'   out2 <- moead(preset   = preset_moead("original"),
#'                 problem  = problem,
#'                 showpars = showpars,
#'                 seed     = 42)
#'
#'   ## Examine the output:
#'   summary(out2)
#'   plot(out2, suppress.pause = TRUE)
#' }
#'
#' # Rerun with MOEA/D-DE configuration and AWT scalarization
#' out3 <- moead(preset   = preset_moead("moead.de"),
#'               problem  = problem,
#'               aggfun   = list(name = "awt"),
#'               stopcrit = list(list(name    = "maxiter",
#'                                    maxiter = 50)),
#'               seed    = seed)
#' plot(out3, suppress.pause = TRUE)

moead <-
  function(preset = NULL,
           # List:  Set of strategy/components
           problem = NULL,
           # List:  MObj problem
           decomp = NULL,
           # List:  decomposition strategy
           aggfun = NULL,
           # List:  scalar aggregation function
           neighbors = NULL,
           # List:  neighborhood assignment strategy
           variation = NULL,
           # List:  variation operators
           update = NULL,
           # List:  update method
           constraint = NULL,
           # List:  constraint handling method
           scaling = NULL,
           # List:  objective scaling strategy
           stopcrit = NULL,
           # List:  stop criteria
           showpars = NULL,
           # List:  echoing behavior
           seed = NULL,
           # Seed for PRNG
           resource.allocation = list(name = "none",
                                      selection = "none",
                                      dt = 2),
           # List:  resource
           # loaded.weights = NULL,
           ...)

# other parameters
  {
    moead.input.pars <- as.list(sys.call())[-1]
    if ("save.env" %in% names(moead.input.pars)) {
      if (moead.input.pars$save.env == TRUE)
        saveRDS(as.list(environment()),
                "moead_env.rds")
    }
    # ============================ Set parameters ============================== #
    if (!is.null(preset)) {
      if (is.null(problem))
        problem   = preset$problem
      if (is.null(decomp))
        decomp    = preset$decomp
      if (is.null(aggfun))
        aggfun    = preset$aggfun
      if (is.null(neighbors))
        neighbors = preset$neighbors
      if (is.null(variation))
        variation = preset$variation
      if (is.null(update))
        update    = preset$update
      if (is.null(scaling))
        scaling   = preset$scaling
      if (is.null(stopcrit))
        stopcrit  = preset$stopcrit
      nullRA <-
        ifelse(
          test = is.null(resource.allocation$name),
          yes = T,
          no = F
        )
    }
    
    
    
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
      if (!exists(".Random.seed"))
        stats::runif(1)
      seed <- .Random.seed
    } else {
      assertthat::assert_that(assertthat::is.count(seed))
      set.seed(seed)               # set PRNG seed
    }
    
    # ============ End Error catching and default value definitions ============ #
    
    # ============================= Algorithm setup ============================ #
    nfe        <- 0              # counter for function evaluations
    time.start <- Sys.time()     # Store initial time
    iter.times <-
      numeric(10000) # pre-allocate vector for iteration times.
    if (is.null(update$UseArchive)) {
      update$UseArchive <- FALSE
    }
    # Archive2 = list(X = NULL, Y = NULL, V = list(v = NULL, Cmatrix = NULL, Vmatrix = NULL))
    # =========================== End Algorithm setup ========================== #
    
    # =========================== Initial definitions ========================== #
    # Generate weigth vectors
    if (decomp$name == "loaded") {
      W <- decomp$W
    }
    else{
      W  <- generate_weights(decomp = decomp,
                             m      = problem$m)
    }
    
    
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
    
    # ========== Initialize Resource Allocation
    init_ra <- resource_allocation_init(resource.allocation, W)
    priority.values <- init_ra$priority.values
    # idx.boundary <- init_ra$idx.boundary
    two_step <- init_ra$two_step
    idx.boundary <- which(W %in% c(0, 1))[1:problem$m]
    
    # ========================= End Initial definitions ======================== #
    
    # ============================= Iterative cycle ============================ #
    keep.running  <- TRUE      # stop criteria flag
    iter          <- 0         # counter: iterations
    
    # ========== Visualization Tools
    # calculating usage of resource by subproblem and any other visualization infoc
    # usage <- rep(TRUE, dim(W)[1])
    # plot.resources <- list(rep(0, dim(W)[1]))
    # plot.paretofront <- list(rep(0, dim(W)[1]))
    # old.nfe = 0
    usage <- list()
    # my.max.1 <- max(Y[, 1])
    # my.max.2 <- max(Y[, 2])
    # mfrow = c(1, 2)
    
    # plot.Y <-
    #   data.frame(
    #     "f1" = Y[, 1] / my.max.1,
    #     "f2" = Y[, 2] / my.max.2,
    #     "was.selected" = as.numeric(rep(1, dim(W)[1]))
    #   )
    # v <-
    #   ggplot(plot.Y, aes(f1, f2, label = was.selected)) + geom_point(aes(color = was.selected), size = 3)
    # v <-
    #   v + coord_cartesian(xlim = c(0, 1.1), ylim = c(0, 1.1)) + ggtitle(paste(iter, resource.allocation$name))
    # v <- v + geom_text()
    # # print(v)
    #
    # w <-
    #   ggplot(plot.Y, aes(f1, f2, label = was.selected)) + geom_point(aes(color = was.selected), size = 3)
    # w <-
    #   w + coord_cartesian(xlim = c(0, 1.1), ylim = c(0, 1.1)) + ggtitle(paste(iter, resource.allocation$name))
    # w <- w + geom_text()
    # grid.arrange(v, w, ncol = 2)
    
    
    Yref <-
      as.matrix(read.table(paste0(
        "../inst/extdata/pf_data/", fun, ".2D.pf"
      )))
    # readline(prompt="Go to next iteration: ")
    idx.parent <- rep(0, nrow(W) + 1)
    s <- list()
    spread <- list()
    pressure.offspring <- list()
    pet <- list()
    offspring.count <- rep(0, nrow(W) + 1)
    # offspring.count <- seq(nrow(W)+1)
    while (keep.running) {
      # Update iteration counter
      iter <- iter + 1
      # print("iter")
      # print(iter)
      # print("nfe")
      # print(nfe-old.nfe)
      # old.nfe = nfe
      if ("save.iters" %in% names(moead.input.pars)) {
        if (moead.input.pars$save.iters == TRUE)
          saveRDS(as.list(environment()),
                  "moead_env.rds")
      }
      
      # ========== Neighborhoods
      # Define/update neighborhood probability matrix
      BP <- define_neighborhood(neighbors = neighbors,
                                v.matrix  = switch(neighbors$name,
                                                   lambda = W,
                                                   x      = X),
                                iter      = iter)
      # ========== Variation
      # Store current popula tion
      Xt <- X
      Yt <- Y
      Vt <- V
      # ========== Resource Allocation - Selecting solutions given Priority Function values
      # find indexes of solutions given their priority value (priority.values)
      select_solutions <- resource_allocation_select(
        iter = iter,
        resource.allocation = resource.allocation,
        W = W,
        priority.values = priority.values,
        idx.boundary = idx.boundary,
        two_step = two_step,
        problem = problem
      )
      # indexes are used by Resource Allocation methods. if none, it is equal to the vector 1
      indexes <- select_solutions$indexes
      iteration_usage = select_solutions$iteration_usage
      # indexes are used by Resource Allocation methods. if none, it is equal to the vector 1
      # Store current population (X) and its objective values (Y)  in temp.X, temp.Y
      # given indexes select solutions to change
      temp.X <- X
      temp.Y <- Y
      # X <- X[indexes,]
      # indexes are used by Resource Allocation methods. if none, it is equal to the vector 1
      # B  <- BP$B.variation[indexes, ]
      B  <- BP$B
      # indexes are used by Resource Allocation methods. if none, it is equal to the vector 1
      # P  <- BP$P[indexes, indexes]
      P  <- BP$P
      # ========== Variation
      # Perform variation
      Xv      <- do.call(perform_variation,
                         args = as.list(environment()))
      X       <- Xv$X
      ls.args <- Xv$ls.args
      nfe     <- nfe + Xv$var.nfe
      var.input.pars <- as.list(sys.call())[-1]
      
      if (variation[[1]]$name == "localsearch") {
        indexes = Xv$which.x
      }
      X <- X[indexes, ]
      # ========== Evaluation
      # Evaluate offspring population on objectives
      YV <- evaluate_population(X       = X,
                                problem = problem,
                                nfe     = nfe)
      Y   <- YV$Y
      V   <- YV$V
      nfe <- YV$nfe
      temp.X[indexes, ] <- X
      X <- temp.X
      temp.Y[indexes, ] <- Y
      Y <- temp.Y
      # ========== Resource Allocation - Combine old solutions with the new ones
      # updating the whole pop with the prioritized solutions in X and in Y
      # indexes are used by Resource Allocation methods. if none, it is equal to the vector 1
      # Y <- Y[indexes, ]
      
      # ========== Scalarization
      # Objective scaling and estimation of 'ideal' and 'nadir' points
      normYs <- scale_objectives(Y       = Y,
                                 Yt      = Yt,
                                 scaling = scaling)
      
      # Scalarization by neighborhood.
      # bigZ is an [(T+1) x N] matrix, in which each column has the T scalarized
      # values for the solutions in the neighborhood of one subproblem, plus the
      # scalarized value for the incumbent solution for that subproblem.
      # B  <- BP$B.scalarize[indexes,]
      
      bigZ <- scalarize_values(
        normYs  = normYs,
        W       = W,
        B       = B,
        aggfun  = aggfun
      )
      fitness <- bigZ[neighbors$T + 1, ]
      
      # Calculate selection indices
      # sel.indx is an [N x (T+1)] matrix, in which each row contains the indices
      # of one neighborhood (plus incumbent), sorted by their "selection quality"
      # (which takes into account both the performance value and constraint
      # handling policy, if any)
      # B  <- BP$B.order[indexes,]
      sel.indx <- order_neighborhood(
        bigZ       = bigZ,
        B          = B,
        V          = V,
        Vt         = Vt,
        constraint = constraint
      )
      # ========== Update
      # Update population
      XY <- do.call(update_population,
                    args = as.list(environment()))
      X       <- XY$X
      Y       <- XY$Y
      V       <- XY$V
      Archive <- XY$Archive
      offspring.count <- XY$offspring.count
      # idx.parent <- idx.parent + XY$idx.parent
      # ========== Resource Allocation - Update Priority function values
      # bad workaround with the problem of not having this values at the first iterations!
      
      # parameters for NORM and Random
      init_ra$dt.bigZ[[length(init_ra$dt.bigZ) + 1]] <- bigZ
      
      #parameters for RI
      index <- ((iter - 1) %% resource.allocation$dt) + 1
      dt.bigZ <- init_ra$dt.bigZ[[index]]
      
      if (is.null(dt.bigZ)) {
        # parameters for NORM and Random - dt.bigz is not used
        if (resource.allocation$name == "random") {
          dt.Y <- Y
          dt.X <- X
        }
        dt.bigZ <- bigZ
      }
      neighbors.T <- neighbors$T
      
      ## parameters for DRA
      # newObj <- bigZ[neighbors$T + 1, ]
      # oldObj <- init_ra$oldObj
      # if(is.null(oldObj)) oldObj <- newObj # means it is not going to be used ever
      
      if (iter > 1 || resource.allocation$name == "random") {
        updates <- resource_allocation_update(
          iter,
          resource.allocation,
          priority.values,
          bigZ,
          dt.bigZ,
          neighbors.T,
          Y,
          dt.Y,
          W,
          X,
          dt.X
        )
        priority.values <- updates$priority.values
        
      }
      
      dt.Y <- Y
      dt.X <- X
      
      if (iter > resource.allocation$dt &&
          resource.allocation$name == "RI") {
        init_ra$dt.bigZ[[index]] <- bigZ
      }
      
      #archive nsga2
      # if (nfe %% 100 == 0){
      #   comb.popX <- rbind(X, Archive$X, Archive2$X)
      #   comb.popY <- rbind(Y, Archive$Y, Archive2$Y)
      #
      #   rankIdxList <- fastNonDominatedSorting(comb.popY)
      #
      #   Archive2$X <- comb.popX[rankIdxList[[1]],]
      #   Archive2$Y <- comb.popY[rankIdxList[[1]],]
      # }
      
      #
      # ========== Visualization Tools
      # calculating usage of resource by subproblem and any other visualization info
      if (nullRA) {
        usage[[length(usage) + 1]] <- rep(1, dim(W)[1])
      }
      else{
        usage[[length(usage) + 1]] <- as.numeric(iteration_usage)
      }
      
      # paretofront <-
      #   cbind(Y, stage = iter)
      # plot.paretofront <- rbind(plot.paretofront, paretofront)
      # resources <-
      #   cbind(Reduce("+", iteration_usage),
      #         stage = iter)
      # plot.resources <- rbind(plot.resources, resources)
      
      # ========== Stop Criteria
      # Calculate iteration time
      elapsed.time <- as.numeric(difftime(
        time1 = Sys.time(),
        time2 = time.start,
        units = "secs"
      ))
      iter.times[iter] <- ifelse(
        test = iter == 1,
        yes = as.numeric(elapsed.time),
        no  = as.numeric(elapsed.time) - sum(iter.times)
      )
      # Verify stop criteria
      keep.running <- check_stop_criteria(stopcrit = stopcrit,
                                          call.env = environment())
      
      # ========== Print
      # Echo whatever is demanded
      print_progress(iter.times, showpars)
      
      # simple pressure
      s[[length(s) + 1]] <- nrow(X) / sum(iter)
      
      # complex pressure (offspring + (traits -> fitness))
      offspring = offspring.count[-1]
      
      thresholdOffspring <- median(offspring)
      thresholdFitness <- median(fitness)
      
      if (thresholdOffspring == 0) {
        thresholdOffspring == 1
      }
      
      A <- 0
      B <- 0
      C <- 0
      D <- 0
      #   thresholdOffspring?
      for (i in 1:dim(W)[1]) {
        if (offspring[i] <= thresholdOffspring) {
          # Low offspring
          if (fitness[i] > thresholdFitness) {
            B = B + 1	# high fitness, low offspring
          }
          else{
            A = A + 1	# low fitness, low offspring
          }
        }
        
        else {
          # High offspring
          if (fitness[i]  > thresholdFitness) {
            D = D + 1 # high fitness, high offspring
          }
          
          else{
            C = C + 1 # low fitness, high offspring
          }
        }
      }
      
      v1 = choose(A+B, A)*choose(C+D,C)/choose(A+B+C+D, A+C)
      
      while(C != 0 && B != 0){
        A = A + 1
        D = D + 1
        B = B - 1
        C = C - 1
        # cat(A, B, C, D, "\n")
      }
      
      v2 = fisher.test(matrix(c(A, C, B, D),nrow=2), alternative = "greater")$p
      
      pet[[length(pet) + 1]] <- -log10(v1+v2)
      
      
      
      pressure.offspring[[length(pressure.offspring) + 1]] <-
        Kendall(offspring, fitness)[1]
      
      # delta spread from NSGA-II - diversity in obj space
      spread[[length(spread) + 1]] <-
        generalizedSpread(Archive$Y, Yref)
      
      # crowding distance from NSGA-II - diversity in obj space
      # cd <- computeCrowdingDistance(t(Archive$Y))
      # cd[which(cd == Inf)] <- 100
      # crowdingDistance[[length(crowdingDistance) + 1]] <- sum(cd)
      # readline(prompt = "Go to next iteration: ")
      #
      #   b <- ggplot(plot.Y, aes(x = 1:dim(W)[1], y = was.selected, shape = was.selected)) + geom_point() +xlab("solution number")+ ggtitle(paste(iter, resource.allocation$name))
      #   png(filename = paste0("~/Downloads/",iter,"_",problem, "_",resource.allocation$name,".png"), width = 8*480, height = 5*480, res=300)
      # grid.arrange(v, b, ncol = 2)
      #   dev.off()
      # eixt()
    }
    # =========================== End Iterative cycle ========================== #
    
    # ================================== Output ================================ #
    # Prepare output
    X <- denormalize_population(X, problem)
    colnames(Y) <- paste0("f", 1:ncol(Y))
    colnames(W) <- paste0("f", 1:ncol(W))
    
    if (!is.null(Archive)) {
      Archive$X <-
        denormalize_population(Archive$X, problem)
      colnames(Archive$Y) <- paste0("f", 1:ncol(Archive$Y))
      Archive$W           <- W
      colnames(Archive$W) <- paste0("f", 1:ncol(Archive$W))
    }
    
    # if (!is.null(Archive2)) {
    #   Archive2$X <-
    #     denormalize_population(Archive2$X, problem)
    #   colnames(Archive2$Y) <- paste0("f", 1:ncol(Archive2$Y))
    #   Archive2$W           <- W
    #   colnames(Archive2$W) <- paste0("f", 1:ncol(Archive2$W))
    # }
    
    # ========== Visualization Tools
    # polishing output names
    # colnames(plot.paretofront) <-
    #   c(paste0("f", 1:ncol(Y)), "stage")
    # colnames(plot.resources) <-
    #   c("Resources", "stage")
    
    # plot.Y <-
    #   data.frame(
    #     "f1" = Y[, 1] / my.max.1,
    #     "f2" = Y[, 2] / my.max.2,
    #     "was.selected" = as.factor(usage[[length(usage)]])
    #   )
    #
    # # print(which(plot.Y$was.selected==1))
    # # my.Y <- cbind(plot.Y$f1, plot.Y$f2)
    # # ndom <- find_nondominated_points(my.Y)
    # # old.plot.Y <- plot.Y[ndom,]
    #
    # plot.Y$was.selected <- factor(plot.Y$was.selected, levels=rev(levels(plot.Y$was.selected)))
    #
    # v <-
    #   ggplot(plot.Y, aes(f1, f2, label = was.selected)) + geom_point(aes(color = was.selected))
    # v <-
    #   v + coord_cartesian(xlim = c(0, 1.1), ylim = c(0, 1.1)) + ggtitle(paste("iter", iter, "nfe", nfe, "method", resource.allocation$name))
    # v <-
    #   v + geom_text() + annotate(
    #     "rect",
    #     xmin =  min(plot.Y$f1),
    #     xmax = max(plot.Y$f1),
    #     ymin =  min(plot.Y$f2),
    #     ymax = max(plot.Y$f2),
    #     fill = "dark grey",
    #     alpha = .3
    #   )
    #
    #
    # new.plot.Y <- plot.Y[which(plot.Y$was.selected == 1), ]
    
    
    
    # w <-
    #   ggplot(new.plot.Y, aes(f1, f2, label = was.selected)) + geom_point(aes(color = was.selected), size = 3)
    # w <-
    #   w + coord_cartesian(xlim = c(0, 1.1), ylim = c(0, 1.1)) + ggtitle(paste("iter", iter, "nfe", nfe, "method", resource.allocation$name))
    # w <-
    #   w + geom_text() + annotate(
    #     "rect",
    #     xmin =  min(plot.Y$f1),
    #     xmax = max(plot.Y$f1),
    #     ymin =  min(plot.Y$f2),
    #     ymax = max(plot.Y$f2),
    #     fill = "dark red",
    #     alpha = .2
    #   ) + annotate(
    #     "rect",
    #     xmin =  min(new.plot.Y$f1),
    #     xmax = max(new.plot.Y$f1),
    #     ymin =  min(new.plot.Y$f2),
    #     ymax = max(new.plot.Y$f2),
    #     fill = "dark grey",
    #     alpha = .5
    #   )
    # grid.arrange(v, w, ncol = 2)
    # dev.off()
    
    # Output
    out <- list(
      X           = X,
      Y           = Y,
      V           = V,
      W           = W,
      Archive     = Archive,
      usage       = usage,
      ideal       = apply(Y, 2, min),
      nadir       = apply(Y, 2, max),
      nfe         = nfe,
      n.iter      = iter,
      time        = difftime(Sys.time(), time.start, units = "secs"),
      seed        = seed,
      inputConfig = moead.input.pars,
      s = s,
      spread = spread,
      pressure.offspring = pressure.offspring,
      pet = pet#,
      # crowdingDistance = crowdingDistance
      # plot.paretofront = plot.paretofront,
      # plot.resources = plot.resources
    )
    class(out) <- c("moead", "list")
    
    return(out)
    # ================================ End Output ============================== #
  }
