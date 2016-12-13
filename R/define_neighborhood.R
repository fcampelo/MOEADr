#' Calculate neighborhood relations
#'
#' Calculates neighborhood relations for the MOEADr package
#'
#' This routine calculates the neighborhood relations for the MOEA/D. The
#' following methods are currently available:
#'
#' #' \itemize{
#'    \item \code{neighbors$name = "lambda"}: defines the neighborhood using the
#'    distance matrix for the weight vectors. The calculation is performed only
#'    once for the entire run.
#'    \item \code{neighbors$name = "x"}: defines the neighborhood using
#'    the distance matrix for the incumbent solution associated with each
#'    subproblem. In this case the calculation is performed at each iteration.
#' }
#'
#' The number of neighbors declared in \code{neighbors$T} must be smaller than
#' the number of subproblems (defined in \code{generate_weights}).
#'
#' The probability of sampling from the neighborhood when performing variation,
#' \code{neightbors$delta.p}, must be a scalar value between 0 and 1.
#'
#' @param neighbors List containing the decomposition method parameters.
#' If \code{NULL} the function searches for \code{neighbors} in the calling
#' environment.
#' @param v.matrix matrix of vectors for defining neighborhoods. If NULL it is
#' captured directly from the calling environment, depending on
#' \code{neighbors$name}).
#'
#' @return List containing the matrix of selection probabilities (\code{P}) and
#' the matrix of neighborhoods (\code{B}).
#'
#' @export

define_neighborhood <- function(neighbors = NULL,
                                v.matrix  = NULL){
  # Capture calling environment
  call.env <- parent.frame()

  # Capture "neighbors" from calling environment if needed
  if (is.null(neighbors)) {
    assertthat::assert_that(assertthat::has_name(call.env, "neighbors"))
    neighbors <- call.env$neighbors
  }

  # ========== Error catching and default value definitions
  valid.methods <- c("lambda", "x")
  assertthat::assert_that(
    all(assertthat::has_name(neighbors, c("name", "T"))),
    neighbors$name %in% valid.methods,
    assertthat::is.count(neighbors$T),
    neighbors$T < nrow(call.env$X),
    is.numeric(neighbors$delta.p),
    length(neighbors$delta.p) == 1,
    is_within(neighbors$delta.p, 0, 1, strict = FALSE))

  if (call.env$iter == 1 || neighbors$name != "lambda"){
    # Define base matrix for defining neighborhood
    if(is.null(v.matrix)) v.matrix <- switch(neighbors$name,
                                             lambda = call.env$W,
                                             x      = call.env$X)

    # Calculate neighborhood matrix
    B <- cbind(1:nrow(v.matrix),
               FNN::get.knn(data      = v.matrix,
                            k         = neighbors$T - 1)$nn.index)
    np  <- nrow(v.matrix)
    P   <- matrix((1 - neighbors$delta.p) / (np - neighbors$T),
                  nrow = np, ncol = np)
    val <- neighbors$delta.p / neighbors$T
    P   <- do.call(rbind,
                   lapply(1:np,
                          FUN = function(i, p, b, val){p[i, b[i, ]] <- val; p[i, ]},
                          p   = P,
                          b   = B,
                          val = val))
  } else {
    # just get the existing matrix
    all(assertthat::has_name(call.env, c("P", "B")))
    B <- call.env$B
    P <- call.env$P
  }

  return(list(B = B, P = P))
}
