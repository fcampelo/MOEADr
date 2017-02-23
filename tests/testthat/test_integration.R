# Test integration of components
context("MOEADr Integration")

# ====================
# Tests in this file are intended to evaluate whether all components of the
# MOEADr framework can be combined without throwing errors.
#
# These tests can take a while - they are NOT intended at validating the
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
                                    H    = 29),
                     decomp2 = list(name = "msld",
                                    H    = c(5, 3),
                                    tau  = c(.9, .5)),
                     decomp3 = list(name = "uniform",
                                    N    = 40))
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
                                         T       = 8,
                                         delta.p = 0.9),
                       neighbors2 = list(name    = "x",
                                         T       = 10,
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
       tau.ls   = 4),
  list(name     = "localsearch",
       type     = "tpqa",
       gamma.ls = .2))

test.variations <- list(variation1 = c(varops, lsops[1]),
                        variation2 = c(varops, lsops[2]))

# ====================


# ====================
# 6. update
test.updates <- list(update1 = list(name       = "best",
                                    Tr         = 5,
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
                                             maxiter = 10)),
                       stopcrit2 = list(list(name    = "maxeval",
                                             maxeval = 200)),
                       stopcrit3 = list(list(name    = "maxtime",
                                             maxtime = 5)),
                       stopcrit4 = list(list(name    = "maxtime",
                                             maxtime = 5),
                                        list(name    = "maxiter",
                                             maxiter = 10),
                                        list(name    = "maxeval",
                                             maxeval = 100)))

# ====================
# 10. showpars
test.showpars <- list(showpars1 = list(show.iters = "none"),
                      showpars2 = list(show.iters = "numbers",
                                       showevery  = 2),
                      showpars3 = list(show.iters = "dots",
                                       showevery  = 1))

# ====================


# II: RUN ALL COMBINATIONS
# (Get ready for a BIG sequence of nested FOR's)

for (i.problem in seq_along(test.problems)){
  problem     <- test.problems[[i.problem]]
  indx.vec    <- i.problem
  for (i.decomp in seq_along(test.decomps)){
    decomp      <- test.decomps[[i.decomp]]
    indx.vec[2] <- i.decomp
    for (i.aggfun in seq_along(test.aggfuns)){
      aggfun      <- test.aggfuns[[i.aggfun]]
      indx.vec[3] <- i.aggfun
      for (i.neighbors in seq_along(test.neighbors)){
        neighbors   <- test.neighbors[[i.neighbors]]
        indx.vec[4] <- i.neighbors
        for (i.update in seq_along(test.updates)){
          update      <- test.updates[[i.update]]
          indx.vec[5] <- i.update
          for (i.constraint in seq_along(test.constraints)){
            constraint  <- test.constraints[[i.constraint]]
            indx.vec[6] <- i.constraint
            for (i.scaling in seq_along(test.scalings)){
              scaling     <- test.scalings[[i.scaling]]
              indx.vec[7] <- i.scaling
              for (i.stopcrit in seq_along(test.stopcrits)){
                stopcrit    <- test.stopcrits[[i.stopcrit]]
                indx.vec[8] <- i.stopcrit
                for (i.showpars in seq_along(test.showpars)){
                  showpars    <- test.showpars[[i.showpars]]
                  indx.vec[9] <- i.showpars
                  for (i.variation in seq_along(test.variations)){
                    variation    <- test.variations[[i.variation]]
                    indx.vec[10] <- i.variation
                    # testthat::test_that(desc = "MOEADr runs without errors",
                    #                     code = is.list(moead(problem,
                    #                                          decomp,
                    #                                          aggfun,
                    #                                          neighbors,
                    #                                          variation,
                    #                                          update,
                    #                                          constraint,
                    #                                          scaling,
                    #                                          stopcrit,
                    #                                          showpars)))
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
