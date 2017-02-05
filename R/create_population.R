#' Create population
#'
#' Create a population for the MOEADr package
#'
#' This routine creates a population matrix for the MOEA/D. Currently only a
#' multivariate uniform distribution is implemented. All points are created
#' within the standardized space 0 <= x_i <= 1, i = 1,...,n_v.
#'
#' @param N population size
#' @param problem list of named problem parameters. If NULL the function
#' searches for \code{problem} in the calling environment. See Section
#' \code{Problem Description} of the [moead()] documentation for details.
#'
#' @return A population matrix X for the MOEA/D.
#'
#' @export

create_population <- function(N,              # population size
                              problem = NULL) # list of named problem parameters
{
    # Capture calling environment
    call.env <- parent.frame()

    # Capture "problem" from calling environment if needed
    if (is.null(problem)) {
        assertthat::assert_that(assertthat::has_name(call.env, "problem"))
        problem <- call.env$problem
    }

    # ========== Error catching and default value definitions
    assertthat::assert_that(
        all(assertthat::has_name(problem, c("xmax", "xmin", "m", "name"))),
        all(problem$xmin < problem$xmax),
        identical(length(problem$xmax), length(problem$xmin)),
        assertthat::is.count(N),
        assertthat::is.count(problem$m))

    if(assertthat::has_name(problem, "constraint")){
      con <- problem$constraint
      assertthat::assert_that(assertthat::has_name(constraint, "name"))
      if (!is.null(con$epsilon)) {
        assertthat::assert_that(is.numeric(con$epsilon),
                                con$epsilon >= 0)
      }
    }


    # get problem dimension
    prob.dim <- length(problem$xmax)

    return (matrix(stats::runif(N * prob.dim),
                   nrow = N))
}
