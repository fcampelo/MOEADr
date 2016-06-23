context("Scalarization")

# Test setup
set.seed(4321)
f <- matrix(sample.int(100, 6) * runif(6), ncol = 3)
w <- matrix(c(1:3,3:1)/6, ncol = 3, byrow=TRUE)
z <- apply(f, 2, min)

# ========== Expected solutions, calculated the long way

# PBI:  g_i       = d_i^1 + \theta*d_i^2
#       d_i^1 = |(f_i - z)' * w| / ||w||
#       d_i^2 = || f_i - [z + d_i^1 * (w / ||w||)] ||
#
a   <- list(name = "pbi", theta = 5)
ra1 <- numeric(nrow(f))
for (i in 1:nrow(f)){
  wn     <- norm(w[i, ], "2")
  d1     <- as.numeric(abs(t(f[i, ] - z) %*% w[i, ]) / wn)
  d2     <- norm(f[i, ] - (z + d1 * w[i, ]), "2")
  ra1[i] <- d1 + a$theta * d2
}

# Weighted sum: g_i = \sum_{j=1}^{m}w_{ij}f_{ij}
#
b   <- list(name = "ws")
rb1 <- rowSums(f * w)

# Tchebycheff: g_i = \max_{j}(w_{ij} * |f_{ij} - z_j|)
#
c   <- list(name = "wt")
rc2 <- rc1 <- numeric(nrow(f))

for (i in 1:nrow(f)){
  rc1[i] <- max(w[i, ] * abs(f[i, ] - z))
}

# Mod Tchebycheff: g_i   = \max_{j}(\rho_{ij} * |f_{ij} - z_j|)
#                  rho_i = (1/w_i) / \sum_{j=1}^{m}(1/w_{ij})
#
d <- list(name = "mwt")
rd1 <- numeric(nrow(f))
for (i in 1:nrow(f)){
  w[w==0] <- 1e-8
  rho <- (1 / w[i, ]) / sum(1 / w[i, ])
  rd1[i] <- max(rho * abs(f[i, ] - z))
}

# Run tests
testthat::test_that("Scalarization returns correct values", {
  expect_equal(scalarize_values(f, w, a), ra1)
  expect_equal(scalarize_values(f, w, b, z), rb1)
  expect_equal(scalarize_values(f, w, c), rc1)
  expect_error(scalarize_values(f, w, d, 0))
})
