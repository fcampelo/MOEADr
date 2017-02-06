#' Run variation operators
#'
#' Sequentially apply variation operators for the MOEADr package
#'
#' This routine performs the variation block for the MOEA/D. The
#' list of available variation operators can be generated using
#' \code{get_variation_operators()}.
#'
#' @param X Population matrix of the MOEA/D (each row is a candidate solution).
#' If \code{NULL} the function searches for \code{X} in the calling environment.
#' @param P Matrix of probabilities of selection for variation (created by
#' \code{define_neighborhoods}). If \code{NULL} the function searches for
#' \code{P} in the calling environment.
#' @param B Matrix of neighborhood indexes (created by
#' \code{define_neighborhoods}). If \code{NULL} the function searches for
#' \code{B} in the calling environment.
#' @param variation List vector containing the variation operators to be used.
#' See \code{\link{moead}} for details. If \code{NULL} the function searches
#' for \code{variation} in the calling environment.
#'
#' @return Modified population matrix X
#'
#' @export

perform_variation <- function(X         = NULL,
                              P         = NULL,
                              B         = NULL,
                              variation = NULL,
                              ...){

  # Capture calling environment
  call.env <- parent.frame()

  # Capture "variation" from calling environment (if needed)
  if (is.null(variation)) {
    assertthat::assert_that(assertthat::has_name(call.env, "variation"))
    variation <- call.env$variation
  }

  # Capture "X" from calling environment (if needed)
  if (is.null(X)) {
    assertthat::assert_that(assertthat::has_name(call.env, "X"))
    X <- call.env$X
  }

  # Capture "P" from calling environment (if needed)
  if (is.null(P)) {
    assertthat::assert_that(assertthat::has_name(call.env, "P"))
    P <- call.env$P
  }

  # Capture "B" from calling environment (if needed)
  if (is.null(B)) {
    assertthat::assert_that(assertthat::has_name(call.env, "B"))
    B <- call.env$B
  }

  # Preserve the original elements of X (prior to variation - used in some
  # variation operators such as binomial recombination)
  Xt <- X

  # Assert that all elements of "variation" have a "name" field
  .ignore <- lapply(variation,
                    FUN = function(x){
                      assertthat::assert_that(assertthat::has_name(x, "name"))})


  # ==================== LOCAL SEARCH SETUP ==================== #
  # Check if local search is part of the variation stack,
  # and treat it accordingly
  lsi <- which(sapply(variation, FUN = function(x){x$name}) == "localsearch")
  if (length(lsi) > 0){
    localsearch      <- variation[[lsi]]
    variation[[lsi]] <- NULL
    valid.types      <- gsub(" ", "", get_localsearch_methods()[,1])
    type             <- localsearch$type
    tau.ls           <- localsearch$tau.ls
    gamma.ls         <- localsearch$gamma.ls
    unsync           <- localsearch$unsync
    trunc.x          <- localsearch$trunc.x

    # ========== Error catching and default value definitions
    assertthat::assert_that(!is.null(tau.ls)  || !is.null(gamma.ls))
    if(is.null(tau.ls)) tau.ls     <- Inf
    if(is.null(gamma.ls)) gamma.ls <- 0
    if(is.null(unsync)) unsync     <- TRUE
    if(is.null(trunc.x)) trunc.x   <- TRUE

    assertthat::assert_that(assertthat::has_name(call.env, "iter"),
                            type %in% valid.types,
                            assertthat::is.count(tau.ls),
                            is_within(gamma.ls, 0, 1),
                            is.logical(unsync),
                            is.logical(trunc.x))

    # ==========

    # Make the necessary preparations in the first iteration
    if (call.env$iter == 1){
      if (unsync) {
        first.ls <- sample.int(n = tau.ls,
                               size = nrow(X),
                               replace = TRUE)
      } else first.ls <- rep(tau.ls, times = nrow(X))

      call.env$ls.args          <- localsearch
      call.env$ls.args$name     <- NULL
      call.env$ls.args$tau.ls   <- tau.ls
      call.env$ls.args$gamma.ls <- gamma.ls
      call.env$ls.args$unsync   <- unsync
      call.env$ls.args$trunc.x  <- trunc.x
      call.env$ls.args$first.ls <- first.ls
    }
  }
  # ================== END LOCAL SEARCH SETUP ================== #

  # ========= PERFORM VARIATION (EXCEPT LOCAL SEARCH) ========== #
  for (i in seq_along(variation)){
    # Assemble function name
    opname       <- paste0("variation_", variation[[i]]$name)

    # Update list of function inputs
    varargs      <- variation[[i]]
    varargs$name <- NULL
    varargs$X    <- X
    varargs$Xt   <- Xt
    varargs$P    <- P
    varargs$B    <- B

    # Perform i-th variation operator
    X <- do.call(opname,
                 args = varargs)
  }
  # ============ END VARIATION (EXCEPT LOCAL SEARCH) ============= #


  # ======================= LOCAL SEARCH ========================= #
  if (length(lsi) > 0){

    # Flag subproblems that will undergo local search in a given iteration
    do.ls <- runif(nrow(X)) <= rep(gamma.ls, times = nrow(X)) |
      (call.env$iter + call.env$first.ls - 1) %% tau.ls == 0

    varargs          <- call.env$ls.args
    varargs$X        <- X
    varargs$Xt       <- Xt
    varargs$Yt       <- Yt
    varargs$B        <- B
    varargs$which.x  <- do.ls
    X <- do.call("variation_localsearch",
                 args = call.env$ls.args)
  }



  return(X)
}
