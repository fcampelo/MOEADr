#' Run variation operators
#'
#' Sequentially apply variation operators for the MOEADr package
#'
#' This routine performs the variation block for the MOEA/D. The
#' list of available variation operators can be generated using
#' [get_variation_operators()].
#'
#' If the `localsearch` operator is included, it is executed whenever its
#' conditions (period of occurrence or probability of occurrence) are verified.
#' See [variation_localsearch()] for details.
#'
#'
#' @param X Population matrix of the MOEA/D (each row is a candidate solution).
#' @param variation List vector containing the variation operators to be used.
#' See [moead()] for details.
#' @param iter iterations counter of the [moead()] function.
#' @param ... other parameters to be passed down to the individual variation
#' operators (see documentation of the specific `variation_`**xyz**`()`
#' functions for details)
#'
#' @return List object containing a modified population matrix `X`, a
#' local search argument list `ls.arg`, and the number of function evaluations
#' used by the variation operators, `var.nfe`.
#'
#' @export

perform_variation <- function(variation, X, iter, ...){

  # Get all input parameters
  # var.input.pars <- as.list(environment()) # <------ for debugging
  var.input.pars <- as.list(sys.call())[-1]

  # Assert that all elements of "variation" have a "name" field
  .ignore <- lapply(variation,
                    FUN = function(x){
                      assertthat::assert_that(assertthat::has_name(x, "name"))})


  # ==================== LOCAL SEARCH SETUP ==================== #
  # Check if local search is part of the variation stack,
  # and treat it accordingly
  lsi <- which(sapply(variation, FUN = function(x){x$name}) == "localsearch")
  if (length(lsi) == 0){
    ls.args        <- NULL
    variation.nols <- variation
  } else {
    if("ls.args" %in% names(var.input.pars)) {
      ls.args <- var.input.pars$ls.args
    } else {
      ls.args      <- variation[[lsi]]
      valid.types  <- gsub(" ", "", get_localsearch_methods()[,1])

      # ========== Error catching and default value definitions
      assertthat::assert_that(
        !is.null(ls.args$tau.ls)  | !is.null(ls.args$gamma.ls))

      if(is.null(ls.args$tau.ls))   ls.args$tau.ls   <- 1e9
      if(is.null(ls.args$gamma.ls)) ls.args$gamma.ls <- 0
      if(is.null(ls.args$unsync))   ls.args$unsync   <- TRUE
      if(is.null(ls.args$trunc.x))  ls.args$trunc.x  <- TRUE

      assertthat::assert_that(
        "iter" %in% names(var.input.pars),
        ls.args$type %in% valid.types,
        assertthat::is.count(ls.args$tau.ls),
        is_within(ls.args$gamma.ls, 0, 1, strict = FALSE),
        is.logical(ls.args$unsync),
        is.logical(ls.args$trunc.x))
    }

    # ==========

    # Make the necessary preparations in the first iteration
    if (iter == 1){
      # Define iteration for the first occurrence of local search (if tau.ls is
      # defined). It never happens in the very first iteration.
      if (ls.args$unsync && !is.infinite(ls.args$tau.ls)) {
        first.ls <- sample.int(n       = ls.args$tau.ls - 1,
                               size    = nrow(X),
                               replace = TRUE)
      } else first.ls <- rep(ls.args$tau.ls,
                             times = nrow(X))

      ls.args$name     <- NULL
      ls.args$first.ls <- first.ls
    }

    # remove local search from the general operator stack
    variation.nols <- variation[-lsi]
  }
  # ================== END LOCAL SEARCH SETUP ================== #


  # ========= PERFORM VARIATION (EXCEPT LOCAL SEARCH) ========== #
  var.nfe          <- 0
  X                <- var.input.pars$X
  var.input.pars$X <- NULL
  for (i in seq_along(variation.nols)){
    # Assemble function name
    opname <- paste0("variation_", variation.nols[[i]]$name)

    # Update list of function inputs
    var.args <- c(var.input.pars, variation.nols[[i]], list(X = X))

    # Perform i-th variation operator
    X <- do.call(opname,
                 args = var.args)

    if (is.list(X)){
      var.nfe <- var.nfe + X$nfe
      X       <- X$X
    }
  }
  # ============ END VARIATION (EXCEPT LOCAL SEARCH) ============= #


  # ======================= LOCAL SEARCH ========================= #
  if (length(lsi) > 0){

    # Flag subproblems that will undergo local search in a given iteration
    # (based on both the LS period and LS probability criteria)
    # (local search never happens in the very first iteration)
    which.tau   <- ((iter + ls.args$first.ls - 1) %% ls.args$tau.ls == 0)
    which.gamma <- stats::runif(nrow(X)) <= rep(ls.args$gamma.ls,
                                                times = nrow(X))
    which.x     <-  (which.tau | which.gamma) & (iter != 1)

    if(any(which.x)){
      # Prepare argument list for local search
      ls.args2          <- c(var.input.pars, ls.args)
      ls.args2$which.x  <- which.x

      # Perform local search
      Xls <- do.call("variation_localsearch",
                     args = ls.args2)

      if (is.list(Xls)){
        var.nfe <- var.nfe + Xls$nfe
        Xls     <- Xls$X
      }

      # Replace points that underwent local search
      X[which.x, ] <- Xls[which.x, ]
    }
  }

  # Output
  return(list (X       = X,
               ls.args = ls.args,
               var.nfe = var.nfe))
}
