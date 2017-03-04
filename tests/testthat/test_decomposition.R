# Test decomposition methods
context("Decomposition methods")

# Assisting Tests
# Checks if all the rows sum to 1
rows_sum_one <- function(W) {
  sums <- rowSums(W)
  names(sums) <- NULL
  ones <- rep(1, nrow(W))
  all.equal(sums, ones)
}

# Checks if number of dimensions is correct
correct_dimensions <- function(W, rows, cols) {
  nrow(W) == rows && ncol(W) == cols
}


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

testthat::test_that("SLD decomposition returns unitary weight vectors", {
  testthat::expect_true(rows_sum_one(generate_weights(decomp.sld1, m1)))
  testthat::expect_true(rows_sum_one(generate_weights(decomp.sld2, m2)))
})

# 2: MSLD decomposition
decomp.msld1 <- list(name = "msld", H = c(1, 2, 3), tau=c(0.3, 0.3, 0.4))
msld1          <- 4
decomp.msld2 <- list(name = "msld", H = c(1, 2, 3), tau=c(1 / 6, 2 / 6, 3 / 6))
msld2          <- 4
decomp.msld3 <- list(name = "msld", H = c(5, 3, 2), tau=c(0.5, 0.3, 0.2))
msld3          <- 2
res3           <- cbind(Var1=c(0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.35, 0.45, 0.55, 0.65, 0.40, 0.50, 0.60),
                        VarLast=c(0.75, 0.65, 0.55, 0.45, 0.35, 0.25, 0.65, 0.55, 0.45, 0.35, 0.60, 0.50, 0.40))

testthat::test_that("MSDL rejects invalid inputs", {
  testthat::expect_error(generate_weights(decomp.msdl1, msld1))
})
testthat::test_that("MSDL returns correct size", {
  testthat::expect_equal(dim(generate_weights(decomp.msld2, msld2)),
                         c(34,4))
})
testthat::test_that("MSDL returns correct values", {
  testthat::expect_equal(generate_weights(decomp.msld3, msld3),
                         res3)
})

testthat::test_that("MSLD returns unitary weight vectors", {
  testthat::expect_true(rows_sum_one(generate_weights(decomp.msld2, msld2)))
  testthat::expect_true(rows_sum_one(generate_weights(decomp.msld3, msld3)))
})

# 3: Uniform decomposition
decomp.unif1 <- list(name = "uniform", N = 10)
# decomp.unif2 <- list(name = "uniform", N = 200) <- Takes WAY too long
decomp.unif3 <- list(name = "uniform", N = 32)
unif_obj <- 4

testthat::test_that("Uniform returns unitary weight vectors", {
  testthat::expect_true(rows_sum_one(generate_weights(decomp.unif1, unif_obj)))
  # testthat::expect_true(is_valid_decomposition(generate_weights(decomp.unif2, unif_obj)))
  testthat::expect_true(rows_sum_one(generate_weights(decomp.unif3, unif_obj)))
})

# Issue 34 on decomposition - Wrong matrix dimensions when number of subobjectives is 2
testthat::test_that("Uniform returns correct dimensions", {
  testthat::expect_true(correct_dimensions(generate_weights(list(name = "uniform", N = 10), 3), 10, 3))
  testthat::expect_true(correct_dimensions(generate_weights(list(name = "uniform", N = 10), 2), 10, 2))
})

testthat::test_that("Uniform return matrix sum to one", {
  testthat::expect_true(rows_sum_one(generate_weights(list(name = "uniform", N = 10), 3)))
  testthat::expect_true(rows_sum_one(generate_weights(list(name = "uniform", N = 10), 2)))
})
