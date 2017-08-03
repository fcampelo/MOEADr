#' plot.moeadoutput
#'
#' S3 method for plotting the output of an moead run
#'
#' This function plots the pareto front of the solution set found by moead.
#' By default, the first and second objectives are plotted, but other objectives
#' can be specified as parameters.
#'
#' @export
plot.moeadoutput <- function(output, d = c(1,2), ...) {

  # TODO: Stub, improve the below

  plot(output$Y[, d[1]], output$Y[, d[2]],
       xlab = paste("Objective",d[1]), ylab = paste("Objective",d[2]),
       ...)

}
