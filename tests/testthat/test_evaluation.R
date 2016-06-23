context("Evaluation")
library(emoa)
prob1 <- list(name = "UF",
              xmin = rep(0, 10), xmax = rep(1, 10),
              nobj = 2, prob.number = 2)
X1 <- create_population(40, prob1)


prob2 <- list(name = "UF",
              xmin = rep(0, 10), xmax = rep(1, 10),
              nobj = 3, prob.number = 8)
X2 <- create_population(100, prob2)

testthat::test_that("Evaluation returns correct dimensions", {
  expect_equal(dim(evaluate_population(X1, prob1)),
               c(nrow(X1), prob1$nobj))
  expect_equal(dim(evaluate_population(X2, prob2)),
               c(nrow(X2), prob2$nobj))
})
