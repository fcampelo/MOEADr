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
#' **Warning**: this routine (and the ones called within it) may access (but
#' not directly modify) variables from the calling environment, depending on
#' the specific operators being performed.
#'
#' @param X Population matrix of the MOEA/D (each row is a candidate solution).
#' @param P Matrix of probabilities of selection for variation (created by
#' [define_neighborhood()]).
#' @param B Matrix of neighborhood indexes (created by [define_neighborhood()]).
#' @param W matrix of weights (created by [generate_weights()]).
#' @param variation List vector containing the variation operators to be used.
#' See [moead()] for details.
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @return List object containing a modified population matrix `X` and a
#' local search argument list `ls.arg`.
#'
#' @export

perform_variation <- function(X, P, B, W, variation, ...){

  # Get calling environment
  call.env <- parent.frame()

  # Preserve the original matrix (used in some variation operators such as
  # binomial recombination)
  Xt <- X

  # Assert that all elements of "variation" have a "name" field
  .ignore <- lapply(variation,
                    FUN = function(x){
                      assertthat::assert_that(assertthat::has_name(x, "name"))})


  # ==================== LOCAL SEARCH SETUP ==================== #
  # Check if local search is part of the variation stack,
  # and treat it accordingly
  lsi <- which(sapply(variation, FUN = function(x){x$name}) == "localsearch")
  if (length(lsi) == 0){
    ls.args <- NULL
  } else {
    if("ls.args" %in% names(call.env)) {
      ls.args <- call.env(ls.args)
    } else {
      ls.args      <- variation[[lsi]]
      valid.types  <- gsub(" ", "", get_localsearch_methods()[,1])

      # ========== Error catching and default value definitions
      assertthat::assert_that(
        !is.null(ls.args$tau.ls)  | !is.null(ls.args$gamma.ls))

      if(is.null(ls.args$tau.ls))   ls.args$tau.ls   <- Inf
      if(is.null(ls.args$gamma.ls)) ls.args$gamma.ls <- 0
      if(is.null(ls.args$unsync))   ls.args$unsync   <- TRUE
      if(is.null(ls.args$trunc.x))  ls.args$trunc.x  <- TRUE

      assertthat::assert_that(
        assertthat::has_name(call.env, "iter"),
        ls.args$type %in% valid.types,
        assertthat::is.count(ls.args$tau.ls),
        is_within(ls.args$gamma.ls, 0, 1, strict = FALSE),
        is.logical(ls.args$unsync),
        is.logical(ls.args$trunc.x))
    }

    # ==========

    # Make the necessary preparations in the first iteration
    if (call.env$iter == 1){
      # Define iteration for the first occurrence of local search (if tau.ls is
      # defined). It never happens in the very first iteration.
      if (ls.args$unsync) {
        first.ls <- 1 + sample.int(n       = ls.args$tau.ls - 1,
                                   size    = nrow(X),
                                   replace = TRUE)
      } else first.ls <- rep(ls.args$tau.ls,
                             times = nrow(X))

      ls.args$name     <- NULL
      ls.args$first.ls <- first.ls
    }

    # remove local search from the general operator stack
    variation[[lsi]] <- NULL
  }
  # ================== END LOCAL SEARCH SETUP ================== #

  # ========= PERFORM VARIATION (EXCEPT LOCAL SEARCH) ========== #
  for (i in seq_along(variation)){
    # Assemble function name
    opname       <- paste0("variation_", variation[[i]]$name)

    # Update list of function inputs
    varargs          <- variation[[i]]
    varargs$X        <- X
    varargs$Xt       <- Xt
    varargs$P        <- P
    varargs$B        <- B
    varargs$call.env <- call.env

    # Perform i-th variation operator
    X <- do.call(opname,
                 args = varargs)
  }
  # ============ END VARIATION (EXCEPT LOCAL SEARCH) ============= #


  # ======================= LOCAL SEARCH ========================= #
  if (length(lsi) > 0){

    # Flag subproblems that will undergo local search in a given iteration
    # (based on both the LS period and LS probability criteria)
    which.tau   <- (call.env$iter + ls.args$first.ls - 1) %% ls.args$tau.ls == 0
    which.gamma <- stats::runif(nrow(X)) <= rep(ls.args$gamma.ls,
                                                times = nrow(X))
    which.x     <-  which.tau | which.gamma

    # Prepare argument list for local search
    varargs          <- c(ls.args, call.env)
    varargs$which.x  <- which.x

    # Perform local search
    Xls <- do.call("variation_localsearch",
                   args = varargs)

    # Replace points that underwent local search
    X[which.x, ] <- Xls[which.x, ]
  }

  # Output
  return(list (X       = X,
               ls.args = ls.args))
}
