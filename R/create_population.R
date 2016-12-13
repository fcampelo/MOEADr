#' Create population
#'
#' Create a population for the MOEADr package
#'
#' This routine creates a population matrix for the MOEA/D. Currently only a
#' multivariate uniform distribution is implemented. The points are created
#' within the space 0 <= x_i <= 1, i = 1,...,n_v.
#'
#' @param N population size
#' @param problem list of named problem parameters. See \code{\link{moead}} for
#' details. If NULL the function searches for \code{problem} in the calling
#' environment.
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


    # get problem dimension
    prob.dim <- length(problem$xmax)

    return (matrix(stats::runif(N * prob.dim),
                   nrow = N))
}
