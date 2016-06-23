#' Calculate decomposition vectors
#'
#' Calculates decomposition vectors for the MOEA/D.
#'
#' This routine calculates the decomposition vectors for the MOEA/D. The
#' following methods for the generation of the weight vectors are currently
#' available:
#' \itemize{
#'    \item \code{$name = "Das"}: uses the method of Das and Dennis (1998). If
#'        this method is chosen, the function requires an additional field
#'        \code{decopars$H} containing a nonnegative integer value used to
#'        calculate the number of decomposition vectors,
#'        \eqn{n_{vecs} = \binom{(H + m - 1),(m - 1)}}
#'    \item \code{$name = "Li"}: uses the method of Li et al. (2014). If this
#'        method is chosen, the function requires additional fields
#'        \code{decopars$H} (similar to the one for the "Das" method), and
#'        \code{decopars$tau}, containing a contraction factor between 0 and 1.\cr
#'        If \code{m <= 6} this method is identical to "Das".\cr
#'        If \code{m > 6} the input \code{decopars$H} must be a 2-value vector.
#'        See Li et al. (2014) for details.
#'    \item \code{$name = "Uniform"}: Uniformly distributed vectors. If
#'        this method is chosen, the function requires an additional field
#'        \code{decopars$nvecs} containing the desired number of weight vectors.
#' }
#'
#' Suggested values for \code{decopars$H} are (use with caution):
#'
#' \tabular{cccc}{
#' \code{m}\tab \code{method}\tab \code{H}\tab n_{vecs}\cr
#'
#' 2    \tab "Das" \tab  \code{99}      \tab 100\cr
#' 3    \tab "Das" \tab  \code{12}      \tab 91\cr
#' 5    \tab "Das" \tab  \code{6}       \tab 210\cr
#' 8    \tab "Li"  \tab  \code{c(3, 2)} \tab 156\cr
#' 10   \tab "Li"  \tab  \code{c(3, 2)} \tab 275\cr
#'}
#'
#' It is important to highlight that the number of vectors generated (based on
#' \code{decopars$H} for the "Das" and "Li" methods, and on
#' \code{decopars$nvecs} for the "Uniform" method) must be greater than the
#' number of neighbors declared in \code{decopars$neighbors}. This function will
#' throw an error if this condition is not verified.
#'
#' @section References:
#' I. Das, J. Dennis (1998), "Normal Boundary Intersection - A New Method
#' for Generating the Pareto Surface in Nonlinear Multicriteria Optimization
#' Problems", SIAM J. Optim., 8(3), 631-657. \cr
#' K. Li et al. (2014), "An Evolutionary Many-Objective Optimization
#' Algorithm Based on Dominance and Decomposition", IEEE Trans. Evol. Comp.,
#' DOI: 10.1109/TEVC.2014.2373386
#'
#' @inheritParams moead
#' @param m Number of objectives of the problem
#'
#' @return Matrix with one decomposition vector per row.
#' @export

decompose_problem <- function(decopars,
                              m)
{
  # ========== Error catching and default value definitions
  valid.methods <- c("Das", "Uniform", "Li")
  assert_that(
    any(decopars$name == valid.methods),
    is.count(m))

  # ========== Generate vectors
  if (decopars$name == "Das"){ # Generate according to Das & Dennis (1998)
    nvecs <- choose(decopars$H + m - 1, m - 1)
    assert_that(
      has_name(decopars, "H"),
      has_name(decopars, "neighbors"),
      is.count(decopars$H),
      decopars$neighbors <= nvecs)

    # Generate decomposition vectors: first (m - 1) columns
    W <- do.call(expand.grid,
                 args = lapply(X = 1:(m - 1),
                               FUN = function(X){
                                 seq(0,
                                     decopars$H) / decopars$H}))
    # Generate m-th column
    W$VarLast <- 1 - rowSums(W)

    # Return only valid weigth vectors
    return(as.matrix(subset(W, W$VarLast >= 0)))
  }

  if (decopars$name == "Li"){ # Generate according to Li et al. (2014)
    tmp <- assert_that(
      has_name(decopars, "H"),
      has_name(decopars, "tau"))

    decopars2       <- decopars
    decopars2$name  <- "Das"
    decopars2$H     <- decopars2$H[1]

    W <- decompose_problem(decopars2, m)

    if (m > 6){
      tmp <- assert_that(length(decopars$H) == 2)
      decopars2$H <- decopars$H[2]
      W <- rbind(W,
                 decompose_problem(decopars2, m) * decopars$tau +
                   (1 - decopars$tau) / m)
    } else if (length(decopars$H) == 2){
      warning("Value of decopar$H[2] dropped.")
    }
    return (W)
  }

  if (decopars$name == "Uniform"){ # Generate randomly
    tmp <- assert_that(
      has_name(decopars, "nvecs"),
      is.count(decopars$nvecs))

    W <- matrix(runif(decopars$nvecs*m),
                ncol = m)

    W <- t(apply(W,
                 MARGIN = 1,
                 FUN = function(x) x/sum(x)))
    return(W)
  }
}
