#' plot.moeadoutput
#'
#' S3 method for plotting _moeadoutput_ objects (the output of [moead()]).
#'
#' @export
plot.moeadoutput <- function(moead.output,
                             useArchive        = FALSE,
                             feasible.only     = TRUE,
                             viol.threshold    = 1e-6,
                             nondominated.only = TRUE,
                             plot.weights      = FALSE,
                             which.objectives  = NULL,
                             color.by.obj      = 1,
                             ...) {


  # Error checking
  assertthat::assert_that(
    "moeadoutput" %in% class(moead.output),
    is.logical(useArchive),
    is.logical(feasible.only),
    is.logical(nondominated.only),
    is.logical(plot.weights),
    is.numeric(viol.threshold) && viol.threshold >= 0,
    is.numeric(color.by.obj),
    color.by.obj %in% seq(1, ncol(moead.output$Y)),
    is.null(which.objectives) || is.count(which.objectives))

  # ===========================================================================
  # Preprocess data for plotting

  Y <- moead.output$Y
  X <- moead.output$X
  V <- moead.output$V
  W <- moead.output$W

  if(useArchive){
    Y <- moead.output$Archive$Y
    X <- moead.output$Archive$X
    V <- moead.output$Archive$V
  }

  if(feasible.only && !is.null(V)){
    feas.indx <- rowSums(V$Vmatrix > viol.threshold) == 0
    Y         <- Y[feas.indx, ]
    X         <- X[feas.indx, ]

    V$Cmatrix <- V$Cmatrix[feas.indx, ]
    V$Vmatrix <- V$Vmatrix[feas.indx, ]
    V$v       <- V$v[feas.indx]
  }

  if(nondominated.only){
    nd.indx   <- find_nondominated_points(Y)
    Y         <- Y[nd.indx, ]
    X         <- X[nd.indx, ]

    if(!is.null(V)){
      V$Cmatrix <- V$Cmatrix[nd.indx, ]
      V$Vmatrix <- V$Vmatrix[nd.indx, ]
      V$v       <- V$v[nd.indx]
    }
  }

  if (!is.null(which.objectives)){
    Y <- Y[, which.objectives]
  }

  ideal <- apply(Y, 2, min)
  nadir <- apply(Y, 2, max)

  # ===========================================================================
  # Determine what is to be plotted
  nobj <- ncol(Y)

  # for 2-objectives, plot points (+ weights, if needed)
  if (nobj == 2){
    dev.hold()
    plot(Y[, 1], Y[, 2], type = "p",
         xlab = colnames(Y)[1], ylab = colnames(Y)[2],
         pch = 16,
         main = "Objectives plot")

    if(plot.weights){
      for (i in 1:nrow(W)){
        termpt <- 1.1 * W[i, ] * (nadir - ideal)
        points(x = c(ideal[1], termpt[1]),
               y = c(ideal[2], termpt[2]),
               type = "l",
               lwd = 0.5)
      }
    }
    dev.flush()
  }

  # for 3-objectives, plot points (+ weights, if needed - looks bad though)
  if (nobj == 3){
    if("scatterplot3d" %in% rownames(installed.packages())){
      dev.hold()
      s3d <- scatterplot3d::scatterplot3d(Y,
                                          pch = 20,
                                          main = "Objectives plot")
      if(plot.weights){
        for (i in 1:nrow(W)){
          termpt <- 1.1 * W[i, ] * (nadir - ideal)
          s3d$points3d(x = c(ideal[1], termpt[1]),
                       y = c(ideal[2], termpt[2]),
                       z = c(ideal[3], termpt[3]),
                       type = "l",
                       lwd = 0.5)
        }
      }
      dev.flush()
    }
  }


  # for 3+objectives, plot parallel coordinates
  if(nobj > 2){
    if("MASS" %in% rownames(installed.packages())){
      dev.hold()
      rbPal <- grDevices::colorRampPalette(c('green','purple'))
      cols  <- rbPal(nrow(Y))

      MASS::parcoord(Y[order(Y[, color.by.obj]), ],
                     lwd = 0.6,
                     var.label = TRUE,
                     col = cols,
                     main = "Parallel coordinates plot")
      dev.flush()
    }

    dev.hold()
    pairs(Y, upper.panel = NULL, pch = 20, main = "2-objective projections")
    dev.flush()
  }
}
