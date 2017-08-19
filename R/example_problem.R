#' Example problem
#'
#' Example problem - minimization of shifted sphere and rastrigin functions.
#'
#' @param X population matrix (see [moead()] for details)
#'
#' @return Matrix of objective function values
#'
#' @export

example_problem <- function(X) {
  t(apply(X,
          MARGIN = 1,
          FUN = function(X){
            c(f.sphere(X), f.rastringin(X))
          }))
}

f.sphere     <- function(x){sum((x + seq_along(x) * 0.1) ^ 2)}

f.rastringin <- function(x){
  x.shift <- x - seq_along(x) * 0.1
  sum((x.shift) ^ 2 - 10 * cos(2 * pi * x.shift) + 10)}
