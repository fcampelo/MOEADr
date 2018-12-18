#' Find non-dominated points
#'
#' Non-dominated point finding for **minimization** problems
#'
#' Non-dominated point finding, based on portions of function _fastNonDominatedSorting_
#' from package NSGA2R
#' ([https://CRAN.R-project.org/package=nsga2R](https://CRAN.R-project.org/package=nsga2R))
#'
#' @param Y row matrix of points in the space of objectives.
#'
#' @return logical vector of length `nrow(Y)` indicating the nondominated points
#' as `TRUE`.
#'
#' @examples
#' Y  <- matrix(runif(200), ncol = 2)
#' nd <- find_nondominated_points(Y)
#' plot(Y[, 1], Y[, 2], type = "p", pch = 20, las = 1)
#' points(Y[nd, 1], Y[nd, 2], type = "p", pch = 16, col = 2, cex = 1.5)
#'
#' @export

find_nondominated_points <- function(Y) {

  idxDominators <- vector(mode = "list", nrow(Y))
  idxDominatees <- vector(mode = "list", nrow(Y))
  for (i in 1:(nrow(Y) - 1)) {
    for (j in i:nrow(Y)) {
      if (all(Y[i, ] <= Y[j, ]) && any(Y[i, ] != Y[j, ])) {  ## i dominates j
        idxDominators[[j]] = c(idxDominators[[j]], i)
        idxDominatees[[i]] = c(idxDominatees[[i]], j)
      } else if (all(Y[j, ] <= Y[i, ]) && any(Y[j, ] != Y[i, ])) {  ## j dominates i
        idxDominators[[i]] = c(idxDominators[[i]], j)
        idxDominatees[[j]] = c(idxDominatees[[j]], i)
      }
    }
  }
  return(unlist(lapply(idxDominators,length)) == 0)
}
