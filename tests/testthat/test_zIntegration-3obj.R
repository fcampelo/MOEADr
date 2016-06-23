# Uncomment for testing (warning: takes a long time)

# context("Algorithm integration - 3obj")
#
# source("integration_setup.R")
#
# # Test 3obj
# probpars <- prob_3obj
# for (i in 1:length(decomp_3obj)){
#   decopars <- decomp_3obj[[i]]
#   for (j in 1:length(scal)){
#     scalpars <- scal[[j]]
#     for (k in 1:length(chng)){
#       chngpars <- chng[[k]]
#       for (l in 1:length(stops)){
#         stopcrit <- stops[[l]]
#         testthat::test_that("Decomposition is well integrated for 3 objectives", {
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
