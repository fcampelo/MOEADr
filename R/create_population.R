#' Create population
#'
#' Create a population for the MOEADr package
#'
#' This routine creates a population matrix for the MOEA/D. Currently only a
#' multivariate uniform distribution is implemented. All points are created
#' within the standardized space \eqn{0 \le x_i \le 1, i = 1,...,n_v}.
#'
#' @param N population size
#' @param problem list of named problem parameters. See Section
#' `Problem Description` of the [moead()] documentation for details.
#'
#' @return A population matrix X for the MOEA/D.
#'
#' @export

create_population <- function(N,       # population size
                              problem) # list of named problem parameters
{

    # ========== Error catching and default value definitions
    assertthat::assert_that(
        all(assertthat::has_name(problem, c("xmax", "xmin", "m", "name"))),
        all(problem$xmin < problem$xmax),
        identical(length(problem$xmax), length(problem$xmin)),
        assertthat::is.count(N),
        assertthat::is.count(problem$m))

    if("constraint" %in% names(problem)){
      con <- problem$constraint
      assertthat::assert_that(assertthat::has_name(con, "name"))
    }


    # get problem dimension
    prob.dim <- length(problem$xmax)

    return (matrix(stats::runif(N * prob.dim),
                   nrow = N))
}
