# Test decomposition methods
context("Scalarization methods")

# Test setup
set.seed(4321)
f <- matrix(sample.int(100, 6) * runif(6), ncol = 3)
w <- matrix(c(1:3,3:1)/6, ncol = 3, byrow = TRUE)
z <- apply(f, 2, min)

# ========== Expected solutions, calculated the long way

# PBI:  g_i       = d_i^1 + \theta*d_i^2
#       d_i^1 = |(f_i - z)' * w| / ||w||
#       d_i^2 = || f_i - [z + d_i^1 * (w / ||w||)] ||
#
aggfun1 <- list(name = "pbi", theta = 5)
res1    <- numeric(nrow(f))
for (i in 1:nrow(f)){
  wn     <- norm(w[i, ], "2")
  d1     <- as.numeric(abs(t(f[i, ] - z) %*% w[i, ]) / wn)
  d2     <- norm(f[i, ] - (z + d1 * w[i, ]), "2")
  res1[i] <- d1 + aggfun1$theta * d2
}


# WEIGHTED SUM: g_i = \sum_{j=1}^{m}w_{ij}f_{ij}
#
res2    <- rowSums(f * w)


# WEIGHTED TCHEBYCHEFF: g_i = \max_{j}(w_{ij} * |f_{ij} - z_j|)
#
res3      <- numeric(nrow(f))

for (i in 1:nrow(f)){
  res3[i] <- max(w[i, ] * abs(f[i, ] - z))
}


# INVERTED PBI
#
# TODO

# ==================================================

# Run tests
testthat::test_that("Scalarization returns correct values", {
  testthat::expect_equal(scalarization_pbi(f, w, z, aggfun1), res1)
  testthat::expect_equal(scalarization_ws(f, w, z), res2)
  testthat::expect_equal(scalarization_wt(f, w, z), res3)
})
