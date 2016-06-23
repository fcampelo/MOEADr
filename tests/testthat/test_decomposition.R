context("Decomposition")

das1 <- list(name = "Das", H = 99, neighbors = 20)
Li1  <- list(name = "Li",  H = 99, neighbors = 20, tau = 0.5)
Uni1 <- list(name = "Uniform",  nvecs = 100, neighbors = 20)
m1   <- 4
das2 <- list(name = "Das", H = 2, neighbors = 10)
Li2  <- list(name = "Li",  H = c(3, 2), tau = 0.25, neighbors = 20)
m2   <- 10

testthat::test_that("Das decomposition returns correct sizes", {
  expect_equal(nrow(decompose_problem(das1, m1)),
               choose(das1$H + m1 - 1, m1 - 1))
  expect_equal(ncol(decompose_problem(das1, m1)), m1)
  expect_equal(nrow(decompose_problem(das2, m2)),
               choose(das2$H + m2 - 1, m2 - 1))
  expect_equal(ncol(decompose_problem(das2, m2)), m2)
})

testthat::test_that("Li decomposition returns correct sizes", {
  expect_equal(nrow(decompose_problem(Li1, m1)),
               choose(Li1$H + m1 - 1, m1 - 1))
  expect_equal(ncol(decompose_problem(Li1, m1)), m1)
  expect_equal(nrow(decompose_problem(Li2, m2)),
               choose(Li2$H[1] + m2 - 1, m2 - 1) + choose(Li2$H[2] + m2 - 1, m2 - 1))
  expect_equal(ncol(decompose_problem(Li2, m2)), m2)
})

testthat::test_that("Uniform decomposition returns correct sizes", {
  expect_equal(nrow(decompose_problem(Uni1, m1)),
               Uni1$nvecs)
  expect_equal(ncol(decompose_problem(Uni1, m1)), m1)
})
