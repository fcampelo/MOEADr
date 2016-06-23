# Uncomment for testing (warning: takes a long time)

# context("Algorithm integration - manyobj")
#
# source("integration_setup.R")
#
# # Test manyobj
# probpars <- prob_manyobj
# for (i in 1:length(decomp_manyobj)){
#   decopars <- decomp_manyobj[[i]]
#   for (j in 1:length(scal)){
#     scalpars <- scal[[j]]
#     for (k in 1:length(chng)){
#       chngpars <- chng[[k]]
#       for (l in 1:length(stops)){
#         stopcrit <- stops[[l]]
#         testthat::test_that("Decomposition is well integrated for many objectives", {
#           skip_on_cran()
#           expect_is(run_moead(probpars, decopars, scalpars,
#                           chngpars, updtpars, showpars,
#                           stopcrit, seed),
#                     "list")
#         })
#       }
#     }
#   }
# }
