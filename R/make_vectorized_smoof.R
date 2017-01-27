#' Make vectorized smoof function
#'
#' Make a vectorized version of test functions available in package "smoof".
#'
#' This routine builds MOEADr-compliant versions of the classic multiobjective
#' test functions available in package smoof. The most commonly used ones are:
#'
#' \itemize{
#'    \item \code{prob.name = ZDT1, ... , ZDT6}, in which case the function
#'          requires additional parameter \code{dimensions} (positive integer)
#'    \item \code{prob.name = DTLZ1, ..., DTLZ7}, in which case the function
#'          requires additional parameters \code{dimensions} (positive integer),
#'          \code{n.objectives} (= 2 or 3) and, for DTLZ4, \code{alpha}
#'          (positive integer, defaults to 100).
#'    \item \code{prob.name = UF}, in which case the function requires
#'          additional parameters \code{dimensions} (positive integer) and
#'          \code{id} (= 1, ..., 10).
#' }
#'
#' @param prob.name name of the problem to build
#' @param ... other parameters passed to each specific function
#'
#' @export
make_vectorized_smoof <- function(prob.name, ...){

  if(!("smoof" %in% rownames(utils::installed.packages()))){
    stop("Please install package 'smoof' to continue")
  }

  my.args            <- as.list(sys.call())[-1]
  my.args$prob.name  <- NULL
  if (length(my.args) == 0) my.args <- list()

  myfun <- do.call(utils::getFromNamespace(x = paste0("make",
                                                      toupper(prob.name),
                                                      "Function"),
                                           ns = "smoof"),
                   args = my.args)
  myfun2 <- function(X, ...){
    t(apply(X,
            MARGIN = 1,
            FUN = myfun))
  }
  return(myfun2)
}

