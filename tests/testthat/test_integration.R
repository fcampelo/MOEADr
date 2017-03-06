# Test integration of components
context("MOEADr Integration")

# ====================
# Tests in this file are intended to evaluate whether all components of the
# MOEADr framework can be combined without throwing errors.
#
# These tests take a LONG time, so they are commented by default. Uncomment the
# nested FOR loops to run the tests (and go have and extended lunch, or maybe
# watch something on Netflix)
#
# These tests are NOT intended at validating the
# technical correctness of implementations or the performance of the MOEADr
# components, but simply to check the integrity of the code under correct use.


# ====================



# I: DEFINE MODULE INSTANCES TO BE TESTED

# ====================
# 1. problem
## Define test function (5-variable, 2-objective DTLZ1)
my.dtlz1 <- function(X, ...){
  # Make DTLS1 function
  smoof_dtlz1 <- smoof::makeDTLZ1Function(dimensions   = 5,
                                          n.objectives = 2)
  # Evaluate points
  Y <- t(apply(X,
               MARGIN = 1,
               FUN = smoof_dtlz1))

  # Return [N x n_f] matrix
  return(Y)
}

# Define list of problem inputs to be tested
test.problems <- list(problem1 = list(name       = "my.dtlz1",
                                      xmin       = rep(0, 5),
                                      xmax       = rep(1, 5),
                                      m          = 2,
                                      constraint = list(
                                        name = "box_constraints")),
                      problem2 = list(name       = "my.dtlz1",
                                      xmin       = rep(0, 5),
                                      xmax       = rep(1, 5),
                                      m          = 2,
                                      constraint = list(
                                        name    = "unitary_constraints",
                                        epsilon = 0.1)))
# ====================


# ====================
# 2. decomp
test.decomps <- list(decomp1 = list(name = "sld",
                                    H    = 9),
                     decomp2 = list(name = "msld",
                                    H    = c(3, 3),
                                    tau  = c(.9, .5)),
                     decomp3 = list(name = "uniform",
                                    N    = 10))
# ====================


# ====================
# 3. aggfun
test.aggfuns <- list(aggfun1 = list(name = "wt"),
                     aggfun2 = list(name = "awt"),
                     aggfun3 = list(name = "pbi", theta = 5),
                     aggfun4 = list(name = "ipbi", theta = 5),
                     aggfun5 = list(name = "ws"))
# ====================


# ====================
# 4. neighbors
test.neighbors <- list(neighbors1 = list(name    = "lambda",
                                         T       = 3,
                                         delta.p = 0.9),
                       neighbors2 = list(name    = "x",
                                         T       = 3,
                                         delta.p = 0.6))
# ====================


# ====================
# 5. variation
varops <- list(
  list(name     = "binrec",
       rho      = 0.8),
  list(name     = "diffmut",
       basis    = "rand",
       Phi      = 0.9),
  list(name     = "diffmut",
       basis    = "mean",
       Phi      = 0.1),
  list(name     = "diffmut",
       basis    = "wgi",
       Phi      = NULL),
  list(name     = "none"),
  list(name     = "polymut",
       etam     = 20,
       pm       = 0.05),
  list(name     = "sbx",
       etax     = 20,
       pc       = 0.9),
  list(name     = "truncate"))

lsops <- list(
  list(name     = "localsearch",
       type     = "dvls",
       tau.ls   = 2),
  list(name     = "localsearch",
       type     = "tpqa",
       gamma.ls = .5))

# Define all combinations of 3 variation operators + 1 local search:
var.cmb <- utils::combn(8,3)
var.cmb <- rbind(cbind(var.cmb, var.cmb),
                 rep(1:2, each = ncol(var.cmb)))

# ====================


# ====================
# 6. update
test.updates <- list(update1 = list(name       = "best",
                                    Tr         = 2,
                                    nr         = 2),
                     update2 = list(name       = "restricted",
                                    nr         = 2),
                     update3 = list(name       = "standard",
                                    UseArchive = TRUE))

# ====================


# ====================
# 7. constraint
test.constraints <- list(constraint1 = list(name = "none"),
                         constraint2 = list(name = "penalty",
                                            beta = 100),
                         constraint3 = list(name = "vbr",
                                            type = "ts"),
                         constraint4 = list(name = "vbr",
                                            type = "sr",
                                            pf   = 0.2),
                         constraint5 = list(name = "vbr",
                                            type = "vt"))
# ====================


# ====================
# 8. scaling
test.scalings <- list(scaling1 = list(name = "none"),
                      scaling2 = list(name = "simple"))

# ====================
# 9. stopcrit
test.stopcrits <- list(stopcrit1 = list(list(name    = "maxiter",
                                             maxiter = 3)),
                       stopcrit2 = list(list(name    = "maxeval",
                                             maxeval = 30)),
                       stopcrit3 = list(list(name    = "maxiter",
                                             maxiter = 3),
                                        list(name    = "maxeval",
                                             maxeval = 30)))

# ====================
# 10. showpars
test.showpars <- list(showpars1 = list(show.iters = "none"))

# ====================

# ====================
# 11. seed
seed = 12345


## II: RUN ALL COMBINATIONS (UNCOMMENT TO RUN)
# require(parallel)
#
# indx.vec <- numeric(10)
# for (i.problem in seq_along(test.problems)){
#   indx.vec[1]    <- i.problem
#   problem        <- test.problems[[i.problem]]
#   for (i.decomp in seq_along(test.decomps)){
#     indx.vec[2] <- i.decomp
#     decomp      <- test.decomps[[i.decomp]]
#     for (i.aggfun in seq_along(test.aggfuns)){
#       indx.vec[3] <- i.aggfun
#       aggfun      <- test.aggfuns[[i.aggfun]]
#       for (i.neighbors in seq_along(test.neighbors)){
#         indx.vec[4] <- i.neighbors
#         neighbors   <- test.neighbors[[i.neighbors]]
#         for (i.update in seq_along(test.updates)){
#           indx.vec[5] <- i.update
#           update      <- test.updates[[i.update]]
#           for (i.constraint in seq_along(test.constraints)){
#             indx.vec[6] <- i.constraint
#             constraint  <- test.constraints[[i.constraint]]
#             for (i.scaling in seq_along(test.scalings)){
#               indx.vec[7] <- i.scaling
#               scaling     <- test.scalings[[i.scaling]]
#               for (i.stopcrit in seq_along(test.stopcrits)){
#                 indx.vec[8] <- i.stopcrit
#                 stopcrit    <- test.stopcrits[[i.stopcrit]]
#                 mcout <- vector(mode = "list", length = length(test.showpars))
#                 for (i.showpars in seq_along(test.showpars)){
#                   indx.vec[9] <- i.showpars
#                   showpars    <- test.showpars[[i.showpars]]
#                   cat("\nTesting ", ncol(var.cmb),
#                       "combinations of ", nrow(var.cmb),
#                       "operators. Index vec = [",
#                       indx.vec[1:9], "]")
#                   mcout[[i.showpars]] <- parallel::mclapply(1:ncol(var.cmb),
#                                                             mc.cores = 7,
#                                                             mc.preschedule = TRUE,
#                                                             FUN = function(i.variation){
#                                                               var.ind      <- var.cmb[-nrow(var.cmb), i.variation]
#                                                               ls.ind       <- var.cmb[nrow(var.cmb), i.variation]
#                                                               variation    <- c(varops[var.ind], lsops[ls.ind])
#                                                               seed         <- as.integer(Sys.time())
#                                                               a            <- moead(problem,
#                                                                                     decomp,
#                                                                                     aggfun,
#                                                                                     neighbors,
#                                                                                     variation,
#                                                                                     update,
#                                                                                     constraint,
#                                                                                     scaling,
#                                                                                     stopcrit,
#                                                                                     showpars,
#                                                                                     seed)
#                                                             })
#                 }
#                 b <- unlist(lapply(mcout, function(x){
#                   a <- unlist(lapply(x, function(y){
#                     if ("try-error" %in% class(y)){
#                       return(FALSE)
#                     } else return(TRUE)}))
#                   return(a)
#                 }))
#                 if (any(!b)) stop("Error in indx.vec = ", indx.vec)
#               }
#             }
#           }
#         }
#       }
#     }
#   }
# }
