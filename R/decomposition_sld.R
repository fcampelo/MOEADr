#' Problem Decomposition using Simplex-lattice Design
#'
#' Problem Decomposition using Simplex-lattice Design for MOEADr package
#'
#' This routine calculates the weight vectors for the MOEA/D using the
#' Simplex-lattice Design.
#'
#' @param decomp list containing the relevant decomposition parameters.
#'  Besides `decomp$name = "sld"`, this method requires the definition of the
#'  following key-value pairs:
#' \itemize{
#'   \item `decomp$H`, decomposition constant. Suggested values for `decomp$H`
#'          are (use with caution):
#'          \tabular{ccccc}{
#'              `m`\tab |\tab`H`\tab |\tab`N`\cr
#'              `2`    \tab  |\tab`99`      \tab |\tab`100`\cr
#'              `3`    \tab  |\tab`12`      \tab |\tab`91`\cr
#'              `5`    \tab  |\tab`6`       \tab |\tab`210`\cr
#'          }
#'          It is important to highlight that the number of vectors generated (`N`) must
#'          be greater than the number of neighbors declared in `neighbors$T`
#'          (see [moead()] for details).
#'   \item \code{decomp$.nobj}: integer value, `decomp$.nobj > 1`. Number of
#'         objectives of the problem.
#' }
#'
#' @param ... other parameters (included for compatibility with generic call)
#'
#' @section References:
#' I. Das, J. Dennis (1998), "Normal Boundary Intersection - A New Method
#' for Generating the Pareto Surface in Nonlinear Multicriteria Optimization
#' Problems", SIAM J. Optim., 8(3), 631-657. DOI: 10.1137/S1052623496307510
#'
#' F. Campelo, L.S. Batista, C. Aranha (2020): The {MOEADr} Package: A
#' Component-Based Framework for Multiobjective Evolutionary Algorithms Based on
#' Decomposition. Journal of Statistical Software \doi{10.18637/jss.v092.i06}\cr
#'
#' @examples
#' decomp <- list(name = "sld", H = 99, .nobj = 2)
#' W <- decomposition_sld(decomp)
#'
#' @export

decomposition_sld <- function(decomp, ...){

    # Error checking
    assertthat::assert_that(
        assertthat::has_name(decomp, "H"),
        assertthat::is.count(decomp$H),
        assertthat::has_name(decomp, ".nobj"),
        assertthat::is.count(decomp$.nobj),
        decomp$.nobj >= 2)

    # Extract variables from decomp
    m <- decomp$.nobj
    H <- decomp$H

    # Calculate number of weight vectors
    N <- choose(H + m - 1, m - 1)
    if (N > 1000) {
      isOK <- readline(paste0("\nThis configuration (H = ", H, ", m = ", m,
                       ") will generate a very large\nnumber of subproblems (N = ", N,
                       ").\nPress <S> to proceed anyway (memory may be insufficient): "))
      if(tolower(isOK) != "s") stop("Operation stopped by user.\nRun choose(H + m - 1, m - 1) to investigate\nthe number of subproblems in SLD.")
    }


    # Generate decomposition vectors: first (m - 1) columns
    W <- do.call(expand.grid,
                 args = lapply(X   = 1:(m - 1),
                               FUN = function(X){seq(0, H) / H}))
    # Generate m-th column
    W$VarLast <- 1 - rowSums(W)

    # Remove invalid vectors
    W <- as.matrix(subset(W, W$VarLast >= 0))

    # Tidy up row numbers
    rownames(W) <- NULL

    return(W)

}
