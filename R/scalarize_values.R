#' Scalarize values for MOEA/D
#'
#' Perform scalarization for the MOEA/D.
#'
#' @inheritParams moead
#' @param Y matrix containing the objective function values to be scalarized
#'          (each row corresponds to a candidate solution)
#' @param W matrix of weights. Must have the same dimensions as Y.
#' @param minP current estimate of the 'ideal' point. Defaults to NULL, in
#'             which case the point is estimated from \code{Y}.
#' @param maxP current estimate of the 'nadir' point. Defaults to NULL, in
#'             which case the point is estimated from \code{Y}.
#'
#' @return Vector of scalarized values.
#' @export

scalarize_values <- function(Y,
                             W,
                             scalpars,
                             minP = NULL,
                             maxP = NULL,
                             eps  = 1e-16)
{
  # ========== Error catching and default value definitions
  assert_that(
    any(scalpars$name == c("ws", "wt", "mwt", "pbi", "ipbi")),
    is.matrix(Y) && is.matrix(W),
    length(scalpars$name) == 1,
    identical(dim(W), dim(Y)),
    (is.null(minP) || length(minP) == ncol(Y)),
    (is.null(maxP) || length(maxP) == ncol(Y)))

  if(has_name(scalpars, "normalize.obj")){
    assert_that(is.flag(scalpars$normalize.obj))
  } else {
    scalpars$normalize.obj <- FALSE
  }
  # ==========

  # Setup ideal and nadir point matrices
  if (is.null(minP)) minP <- getminP(Y)
  if (is.null(maxP)) maxP <- getmaxP(Y)
  minP <- matrix(rep(minP, times = nrow(Y)),
                 nrow  = nrow(Y),
                 byrow = TRUE)
  maxP <- matrix(rep(maxP, times = nrow(Y)),
                 nrow  = nrow(Y),
                 byrow = TRUE)

  # Normalize function values if necessary
  if(scalpars$normalize.obj) {
    Y <- (Y - minP) / (maxP - minP + eps)
    minP <- 0
    maxP <- 1
  }

  if (scalpars$name == "ws"){ # Weighted sum
    Z <- apply(W * Y,
               MARGIN = 1,
               FUN    = sum)

    return(as.numeric(Z))
  }

  if (scalpars$name == "wt"){ # Weighted Tchebycheff
    Z <- apply(W * (Y - minP + eps),
               MARGIN = 1,
               FUN    = max)

    return(as.numeric(Z))
  }

  if (scalpars$name == "mwt"){ # Mod. Tchebycheff [Lopez & Batista 2015]
    # Modify weights
    W[W == 0] <- 1e-8
    Rho <- (1 / W) / rowSums(1 / W)

    # Scalarize using "wt"
    scalpars$name <- "wt"
    Z             <- scalarize_values(Y, Rho, scalpars)

    return(as.numeric(Z))
  }


  if (scalpars$name == "pbi"){ # PBI
    assert_that(has_name(scalpars, "theta"))

    # Norm of the weight vectors
    NormW <- matrix(sqrt(rowSums(W ^ 2)),
                    nrow = nrow(W),
                    ncol = ncol(W),
                    byrow = FALSE)

    # Calculate D1 and D2
    D1 <- matrix(rowSums((Y - minP + eps) * W) / NormW,
                 nrow = nrow(W),
                 ncol = ncol(W),
                 byrow = FALSE)

    D2 <- sqrt(rowSums((Y - minP - D1 * W) ^ 2))

    return(as.numeric(D1[, 1] + scalpars$theta * D2))
  }


  if (scalpars$name == "ipbi"){ # Inverted PBI [Sato 2015]
    assert_that(has_name(scalpars, "theta"))

    # Norm of the weight vectors
    NormW <- matrix(sqrt(rowSums(W ^ 2)),
                    nrow  = nrow(W),
                    ncol  = ncol(W),
                    byrow = FALSE)

    # Calculate D1 and D2
    D1 <- matrix(rowSums((maxP - Y - eps) * W) / NormW,
                 nrow = nrow(W),
                 ncol = ncol(W),
                 byrow = FALSE)

    D2 <- sqrt(rowSums((maxP - Y - D1 * W) ^ 2))

    return(scalpars$theta * D2 - as.numeric(D1[, 1]))
  }
}
