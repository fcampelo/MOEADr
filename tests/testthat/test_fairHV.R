# Test integration of components
context("MOEADr fair HV on Summary")

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



# I: DEFINE VECTOR INSTANCES TO BE TESTED
# ====================
# Vector to be scaled
Y <- sample(x = 1:10000, size = 45)
scaled.Y <- scale_vector(Y)

testthat::test_that("Scale funtion works", {
  testthat::expect_equal(min(scaled.Y),0)
  testthat::expect_equal(max(scaled.Y),1)
})

# II: DEFINE MODULE INSTANCE AND TEST ITS PARAMETERS
ZDT1 <- MOEADr::make_vectorized_smoof(prob.name  = "DTLZ1",
                                      dimensions = 10,
                                      n.objectives = 2)
problem.zdt1  <- list(
  name       = "ZDT1",
  xmin       = rep(0, 10),
  xmax       = rep(1, 10),
  m          = 2
)

set.seed(42)
control <- moead(problem  = problem.zdt1,
                         preset   = preset_moead("moead.de"),
                         showpars = list(show.iters = "dots", showevery = 100),
                         seed     = 42)

ref.points <- create_ref.points(control$H, control$n.problems)
manual.ref.points <- rep(1 + 1/control$H, control$n.problems)
Y <- apply(control$Y, MARGIN = 2, scale_vector)
hv <- emoa::dominated_hypervolume(points = t(Y), ref = ref.points)

# Verify ref.points

testthat::test_that("Scale funtion works", {
  testthat::expect_equal(ref.points, manual.ref.points)
  testthat::expect_equal(length(ref.points), control$n.problems)
  testthat::expect_gte(control$H, 2)
  testthat::expect_equal(control$n.problems, problem.zdt1$m)
  testthat::expect_equal(round(hv, 3), round(1.005362, 3)
)
})
