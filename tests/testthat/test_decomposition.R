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

# 2: MSLD decomposition
# 2.1 checks errors correctly
# 2.2 returns correct size
# 2.3 returns correct value
# 2.4 works even for |H| = 1
decomp.msld1 <- list(name = "msld", H = c(1,2,3), tau=c(0.3,0.3,0.4))
msld1          <- 4
decomp.msld2 <- list(name = "msld", H = c(1,2,3), tau=c(1/6,2/6,3/6))
msld2          <- 4
decomp.msld3 <- list(name = "msld", H = c(5,3,2), tau=c(0.5,0.3,0.2))
msld3          <- 2
res3           <- cbind(Var1=c(0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.35, 0.45, 0.55, 0.65, 0.40, 0.50, 0.60),
                        VarLast=c(0.75, 0.65, 0.55, 0.45, 0.35, 0.25, 0.65, 0.55, 0.45, 0.35, 0.60, 0.50, 0.40))

testthat::test_that("MSDL rejects invalid inputs", {
  testthat::expect_error(generate_weights(decomp.msdl1,msld1))
})
testthat::test_that("MSDL returns correct size", {
  testthat::expect_equal(dim(generate_weights(decomp.msld2,msld2)),
                         c(34,4))
})
testthat::test_that("MSDL returns correct values", {
  testthat::expect_equal(generate_weights(decomp.msld3,msld3),
                         res3)
})
