#' Calculate neighborhood relations
#'
#' Calculates neighborhood relations for the MOEADr package
#'
#' This routine calculates the neighborhood relations for the MOEA/D.
#'
#' **Warning**: this routine may access (but not directly modify) variables
#' from the calling environment.
#'
#' @param neighbors List containing the decomposition method parameters.
#'  This list must contain the following key-value pairs:
#' \itemize{
#'   \item `neighbors$name`, type of neighborhood to use. The following types
#'         are currently available:
#'         \itemize{
#'           \item `neighbors$name = "lambda"`: defines the neighborhood using
#'             the distance matrix for the weight vectors. The calculation is
#'             performed only once for the entire run.
#'           \item `neighbors$name = "x"`: defines the neighborhood using
#'             the distance matrix for the incumbent solution associated with
#'             each subproblem. In this case the calculation is performed at
#'             each iteration.
#' }
#'   \item `neighbors$T`: Neighborhood size. The value of `neighbors$T`
#'          must be smaller than the number of subproblems.
#'   \item `neighbors$delta.p`: Probability of sampling from the neighborhood
#'   when performing variation. Must be a scalar value between 0 and 1.
#' }
#' @param v.matrix matrix of vectors to be used for defining the neighborhoods.
#' @param iter iteration counter of the MOEA/D
#'
#' @return List containing the matrix of selection probabilities (`P`) and
#' the matrix of neighborhoods (`B`).
#'
#' @export

define_neighborhood <- function(neighbors, v.matrix, iter){

  # ========== Error catching and default value definitions
  valid.methods <- c("lambda", "x")
  assertthat::assert_that(
    all(assertthat::has_name(neighbors, c("name", "T"))),
    neighbors$name %in% valid.methods,
    assertthat::is.count(neighbors$T),
    neighbors$T <= nrow(v.matrix),
    is.numeric(neighbors$delta.p),
    length(neighbors$delta.p) == 1,
    is_within(neighbors$delta.p, 0, 1, strict = FALSE))

  if (iter == 1 || neighbors$name != "lambda"){
    BP <- list(B = NULL, P = NULL, fullB = NULL, fullP = NULL)

    # Calculate neighborhood matrix
    BP$fullB <- cbind(1:nrow(v.matrix),
                      FNN::get.knn(data      = v.matrix,
                                   k         = nrow(v.matrix) - 1)$nn.index)
    BP$B <- BP$fullB[, 1:neighbors$T]
    np  <- nrow(v.matrix)
    if (np > neighbors$T){
      BP$P   <- matrix((1 - neighbors$delta.p) / (np - neighbors$T),
                       nrow = np,
                       ncol = np)

      val <- neighbors$delta.p / neighbors$T
      BP$P   <- do.call(rbind,
                        lapply(1:np,
                               FUN = function(i, p, b, val){
                                 p[i, b[i, ]] <- val; p[i, ]},
                               p   = BP$P,
                               b   = BP$B,
                               val = val))
    } else {
      BP$P   <- matrix(1 / np,
                       nrow = np,
                       ncol = np)
    }
    BP$fullP <- BP$P
    BP$fullP[, ] <- 1 / ncol(BP$fullP)

  } else {
    # just get the existing matrix
    call.env <- parent.frame()
    assertthat::assert_that("BP" %in% names(call.env))
    BP <- call.env$BP
  }

  return(BP)
}
