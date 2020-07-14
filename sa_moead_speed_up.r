rm(list = ls(all = TRUE))
pkgs = c("sensitivity", "MOEADr", "smoof", "R.utils", "moobench")
inst = lapply(pkgs, library, character.only = TRUE)
setwd("~/MOEADr/R/")
sourceDirectory(".")
file.sources = list.files(pattern = "*.R")


mySim <- function(r, n) {
  M <- length(r)
  X <- data.frame(matrix(runif(M * n), nrow = n))
  for (m in (1:M)) {
    rm <- r[[m]]
    X[, m] <- X[, m] * (rm$max - rm$min) + rm$min
  }
  return(X)
}
k = 2 * (3-1)
d <- 25
problem <- load.WFG.function("WFG1", 3)
problem.wfg <- problem$problem
pareto.front <- problem$pf
problem.zdt1 <- list(
  name       = "problem.wfg",
  xmin       = rep(0, 25),
  xmax       = rep(1, 25),
  m          = 3
)
sens_analysis <- function(x) {

  N <- dim(x)[1]
  res <- list()
  for (i in 1:N) {

    ### new code
    ## 2. Decomp
    decomp <- list(name = "SLD")
    decomp$H <- 19
    ## 3. Neighbors
    neighbors <- list(name    = "lambda",
                      T       = x[i, 1],
                      #10-40
                      delta.p = x[i, 2])#0.1~1)
    ## 4. Aggfun
    aggfun <- list(name = "wt")
    ## 5. Update
    update <- list(name       = "standard",
                   UseArchive = x[i, 3])
    if (update$name != "standard")
      update$nr <- x[i, 4]
    ## 6. Scaling
    scaling <- list(name = "none")

    ## 7. Constraint
    constraint <- list(name = "none")

    ## 8. Stop criterion
    stopcrit  <- list(list(name    = "maxiter",
                           maxiter = 300))
    ## 9. Echoing
    showpars  <- list(show.iters = "none")
    ## 10. Variation stack
    variation <- list(list(name = "diffmut"),
                      list(name = "polymut"),
                      list(name = "truncate"))

    for (i in seq_along(variation)) {
      if (variation[[i]]$name == "polymut") {
        variation[[i]]$etam <-  x[i, 5]
        variation[[i]]$pm   <-  x[i, 6]
      }
      if (variation[[i]]$name == "diffmut") {
        variation[[i]]$basis <- "rand"
        variation[[i]]$phi   <- x[i, 7]
      }
    }
    results <- moead(
      problem  = problem.zdt1,
      decomp = decomp,
      aggfun = aggfun,
      neighbors = neighbors,
      update = update,
      variation = variation,
      constraint = constraint,
      scaling = scaling,
      showpars = showpars,
      stopcrit = stopcrit
    )

    ref.points <-
      rep(10, problem.zdt1$m)

    out <- list(emoa::dominated_hypervolume(points = t(results$Y), ref.points))
    res <- cbind(res, out)
  }
  return(res)
}


l1 <- list(min = 10, max = 40) # T 1st order bigger
l2 <- list(min = 0.1, max = 1) # delta.p 1st order bigger
l3 <- list(min = 0, max = 1)   # UseArchive
l4 <- list(min = 1, max = 10)   # nr
l5 <- list(min = 0, max = 100)   # polymut.eta
l6 <- list(min = 0, max = 1)   # polymut.pm
l7 <- list(min = 0, max = 1)   # phi 1st order bigger


X1 <- mySim(list(l1, l2, l3, l4, l5, l6, l7), n = 400)
X1[, 1] <- as.integer(X1[, 1])
X1[, 3] <- as.integer(X1[, 3])
X1[, 4] <- as.integer(X1[, 3])
X2 <- mySim(list(l1, l2, l3, l4, l5, l6, l7), n = 400)
X2[, 1] <- as.integer(X2[, 1])
X2[, 3] <- as.integer(X2[, 3])
X2[, 4] <- as.integer(X2[, 4])


mem_sobolEff <- memoise::memoise(sobolEff)
cmp_mem_sobolEff <- compiler::cmpfun(mem_sobolEff)
y <-
  cmp_mem_sobolEff(
    model = sens_analysis,
    X1 = X1,
    X2 = X2,
    order = 1,
    nboot = 100
  )

save(y, file = "first.order")
# png(filename = 'sobolEff_order1.png')
# plot(y)
# dev.off()

z <-
  cmp_mem_sobolEff(
    model = sens_analysis,
    X1 = X1,
    X2 = X2,
    order = 2,
    nboot = 100
  )
save(z, file = "second.order")
# print(z)
# png(filename = 'sobolEff_order2.png')
# plot(z)
# dev.off()
