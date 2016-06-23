# Uncomment for testing (warning: takes a long time)

# context("Algorithm integration - 2obj")
#
# source("integration_setup.R")
#
# # Test 2obj
# probpars <- prob_2obj
# for (i in 1:length(decomp_2obj)){
#   decopars <- decomp_2obj[[i]]
#   for (j in 1:length(scal)){
#     scalpars <- scal[[j]]
#     for (k in 1:length(chng)){
#       chngpars <- chng[[k]]
#       for (l in 1:length(stops)){
#         stopcrit <- stops[[l]]
#         testthat::test_that("Decomposition is well integrated for 2 objectives", {
#           skip_on_cran()
#           expect_is(run_moead(probpars, decopars, scalpars,
#                               chngpars, updtpars, showpars,
#                               stopcrit, seed),
#                     "list")
#         })
#       }
#     }
#   }
# }
