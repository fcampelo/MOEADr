# Depends on package "mco"
mco_zdt <- function(X, prob.number){
  assertthat::assert_that(
    assertthat::is.count(prob.number),
    prob.number <= 3)

  fun.name <- paste0("zdt",
                     prob.number)
  t(apply(X,
          MARGIN = 1,
          FUN = get(fun.name)))
}

# Depends on package "emoa"
UF <- function(X, prob.number){
  assertthat::assert_that(
    assertthat::is.count(prob.number),
    prob.number <= 10)

  fun.name <- paste0("UF",
                     prob.number)
  t(apply(X,
          MARGIN = 1,
          FUN = get(fun.name)))
}


# Depends on package "GPareto"
# Must check the dimension of X
ZDT <- function(X, prob.number){
  assertthat::assert_that(
    assertthat::is.count(prob.number),
    any(prob.number == c(1:4, 6)))

  fun.name <- paste0("ZDT",
                     prob.number)
  do.call(fun.name,
          args = list(x = X))
}


# Depends on package "GPareto"
# ncol(X) >= nobj
DTLZ <- function(X, prob.number, nobj){
  assertthat::assert_that(
    assertthat::is.count(prob.number),
    any(prob.number == c(1:3, 7)),
    assertthat::is.count(nobj),
    nobj <= ncol(X))

  fun.name <- paste0("DTLZ",
                     prob.number)
  do.call(fun.name,
          args = list(x = X))
}

