# Setup
library(GPareto)
library(emoa)

updtpars <- list(method = "standard")
showpars <- list(show.iters = "none", showevery = 1)
seed     <- NULL

# Test options
prob_2obj = list(name = "ZDT", xmin = rep(0, 2), xmax = rep(1, 2), prob.number = 1, nobj = 2)
decomp_2obj <- list(das2  = list(name = "Das", H = 99, neighbors = 25),
                    Li1   = list(name = "Li", H = c(15, 2), neighbors = 10, tau = 0.5),
                    Li2   = list(name = "Li", H = c(25, 2), neighbors = 10, tau = 0.2),
                    unif1 = list(name = "Uniform", nvecs = 100, neighbors = 40),
                    unif2 = list(name = "Uniform", nvecs = 200, neighbors = 10))

prob_3obj = list(name = "DTLZ", xmin = rep(0, 10), xmax = rep(1, 10), nobj = 3, prob.number = 1)
decomp_3obj <- list(das1  = list(name = "Das", H = 12, neighbors = 20),
                    Li1   = list(name = "Li", H = c(4, 2), neighbors = 10, tau = 0.8),
                    Li2   = list(name = "Li", H = c(4, 3), neighbors = 5,  tau = 0.1),
                    unif1 = list(name = "Uniform", nvecs = 100, neighbors = 40),
                    unif2 = list(name = "Uniform", nvecs = 200, neighbors = 10))

prob_manyobj = list(name = "DTLZ", xmin = rep(0, 30), xmax = rep(1, 30), nobj = 8, prob.number = 2)
decomp_manyobj <- list(das1  = list(name = "Das", H = 4, neighbors = 20),
                       Li1   = list(name = "Li", H = c(3, 2), neighbors = 10, tau = 1.2),
                       Li2   = list(name = "Li", H = c(4, 3), neighbors = 10, tau = 0.3),
                       unif1 = list(name = "Uniform", nvecs = 100, neighbors = 40),
                       unif2 = list(name = "Uniform", nvecs = 200, neighbors = 10))


# Scalarization options
scal <- list(
  ws1  = list(name = "ws"),
  wt1  = list(name = "wt"),
  mwt1 = list(name = "mwt"),
  pbi1 = list(name = "pbi", theta = 5),
  pbi2 = list(name = "pbi", theta = 1),
  pbi3 = list(name = "pbi", theta = 20))

# Variation options
chng <- list(
  chng1 = list(sbx     = list(eta = 20, pc = 0.8, eps = 1e-12),
               polymut = list(eta = 20, pm = 0.05)),
  chng2 = list(sbx     = list(eta = 10, pc = 0.5, eps = 1e-8),
               polymut = list(eta = 2, pm = 0.01)),
  chng3 = list(polymut = list(eta = 2, pm = 0.01),
               sbx     = list(eta = 10, pc = 0.5, eps = 1e-10)))

# Stop options
stops <- list(
  stop1 = list(names = "stop_maxiter", maxiter = 100),
  stop2 = list(names = "stop_maxeval", maxeval = 10000),
  stop3 = list(names = c("stop_maxeval", "stop_maxiter"), maxiter = 150, maxeval = 10000))
