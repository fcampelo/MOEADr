#' Create population
#'
#' Create new population for MOEA/D
#'
#' The detailed description comes here...
#'
#' @param popsize population size
#' @param probpars list of named problem parameters (see \code{\link{moead}}).
#'
#' @return A matrix containing the population for the MOEA/D

create_population <- function(popsize,      # population size
                              probpars)     # list of named problem parameters
{
  #Generate population of individuals within the standardized space x \in (0,1)

  # ========== Error catching and default value definitions
  assert_that(
    all(has_name(probpars, c("xmax", "xmin", "nobj", "name"))),
    all(probpars$xmin < probpars$xmax),
    identical(length(probpars$xmax), length(probpars$xmin)),
    is.count(popsize),
    is.count(probpars$nobj))

  # get problem dimension
  prob.dim <- length(probpars$xmax)

  return (matrix(runif(popsize * prob.dim),
                 nrow = popsize))
}
