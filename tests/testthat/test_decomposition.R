# Test decomposition methods
context("Decomposition methods")

# 1: SLD decomposition
decomp.sld1 <- list(name = "sld", H = 99)
m1          <- 4
decomp.sld2 <- list(name = "sld", H = 2)
m2          <- 10

testthat::test_that("SLD decomposition returns correct sizes", {
  testthat::expect_equal(dim(generate_weights(decomp.sld1, m1)),
                         c(choose(decomp.sld1$H + m1 - 1,
                                  m1 - 1),
                           m1))
  testthat::expect_equal(dim(generate_weights(decomp.sld2, m2)),
                         c(choose(decomp.sld2$H + m2 - 1,
                                  m2 - 1),
                           m2))
})
