#' Inverted Generational Distance
#'
#' Calculate IGD
#'
#' @param Y Matrix of points in the objctive space
#' @param Yref Matrix of Pareto-optimal reference points
#'
#' @return igd value (scalar)
#'
#' @export
calcIGD <- function(Y, Yref){
  igd <- sum(apply(Yref, 1,
                   function(yref, Y){
                     min(apply(Y, 1,
                               function(y, yref){
                                 sqrt(sum((y - yref) ^ 2))
                               },
                               yref = yref))
                   },
                   Y = Y)) / nrow(Yref)
  return(igd)
}
