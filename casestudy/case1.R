## Case Study 1 -- simple problem solving
# TODO: add text here -- ideally, the same text that will go on the paper
# probably should replace this with an Rmd file.

# Problem setting -- replace this with an explicit simple problem -- add explanation
library(smoof)
ZDT1 <- make_vectorized_smoof(prob.name  = "ZDT1",
                              dimensions = 30)
problem   <- list(name       = "ZDT1",
                  xmin       = rep(0, 30),
                  xmax       = rep(1, 30),
                  m          = 2)

# Experiment Control parameters -- add explanation
showpars  <- list(show.iters = "dots",
                  showevery  = 10)
seed      <- 42

# Running the program -- add explanation, add details about preset_moead()
out1 <- moead(problem = problem,
              preset = preset_moead("original"), stopcrit = list(list(name = "maxiter", maxiter = 1000)),
              showpars = showpars, seed = seed)

# Output -- add explanation, more output information
summary(out1)
plot(out1)
