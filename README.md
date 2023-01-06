## MOEADr package
[![Build Status](https://api.travis-ci.org/fcampelo/MOEADr.png)](https://travis-ci.org/fcampelo/MOEADr) [![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/MOEADr)](https://CRAN.R-project.org/package=MOEADr)
[![CRAN Downloads](https://cranlogs.r-pkg.org/badges/MOEADr)](https://CRAN.R-project.org/package=MOEADr)

***

[Felipe Campelo](mailto:fcampelo@ufmg.br) 
Department of Computer Science  
Aston University  
Birmingham, UK

[Lucas Batista](mailto:lusoba@ufmg.br)  
Operations Research and Complex Systems Laboratory - ORCS Lab  
Universidade Federal de Minas Gerais  
Belo Horizonte, Brazil

  
[Claus Aranha](mailto:caranha@cs.tsukuba.ac.jp)  
Faculty of Engineering, Information and Systems  
University of Tsukuba  
Tsukuba, Japan

***

**R** package containing a component-based, modular implementation of the Multiobjective Evolutionary Algorithm with Decomposition (MOEA/D) framework. 

The MOEA/D framework is seen as a combination of specific design decisions regarding several independent modules:

- Decomposition strategy;  
- Aggregation function;  
- Objective scaling strategy;  
- Neighborhood assignment strategy;  
- Variation Stack;  
- Update strategy;  
- Constraint handling method;  
- Termination criteria.

This package provides several options for each module, as explained in the documentation of its main function, `MOEADr::moead()`. The input structure of this function is also explained in its documentation. More details on the component-based approach behind the `MOEADr` package are available in our paper, _The MOEADr Package - A Component-Based Framework for Multiobjective Evolutionary Algorithms Based on Decomposition_, available on the ArXiv: [https://arxiv.org/abs/1807.06731](https://arxiv.org/abs/1807.06731).

To install the current release version in your system, simply use:

```
install.packages("MOEADr")
```

For the most up-to-date development version, install the github version using:

```
# install.packages("devtools")
devtools::install_github("fcampelo/MOEADr")
```

## Example

As a simple example, we can reproduce the original MOEA/D (Zhang and Li, 2007) and run it on a 30-variable ZDT1 function:

```
 ## 1: prepare test problem
 library(smoof)
 ZDT1 <- make_vectorized_smoof(prob.name  = "ZDT1",
                               dimensions = 30)

 ## 2: set input parameters
 problem   <- list(name       = "ZDT1",
                   xmin       = rep(0, 30),
                   xmax       = rep(1, 30),
                   m          = 2)
 decomp    <- list(name       = "SLD", H = 99)
 neighbors <- list(name       = "lambda",
                   T          = 20,
                   delta.p    = 1)
 aggfun    <- list(name       = "wt")
 variation <- list(list(name  = "sbx",
                        etax  = 20, pc = 1),
                   list(name  = "polymut",
                        etam  = 20, pm = 0.1),
                   list(name  = "truncate"))
 update    <- list(name       = "standard", 
                   UseArchive = FALSE)
 scaling   <- list(name       = "none")
 constraint<- list(name       = "none")
 stopcrit  <- list(list(name  = "maxiter",
                     maxiter  = 200))
 showpars  <- list(show.iters = "dots",
                   showevery  = 10)
 seed      <- NULL

 ## 3: run MOEA/D
 out1 <- moead(problem = problem, 
               decomp = decomp, aggfun = aggfun, neighbors = neighbors, variation = variation, 
               update = update, constraint = constraint, scaling = scaling, stopcrit = stopcrit,
               showpars = showpars, seed = seed)

 ## 3.1: For your convenience, you can also use the preset_moead() function to reproduce the above setup, 
 ##      and only modify the desired parts:
 
 out2 <- moead(problem = problem,
               preset = preset_moead("original"), 
               stopcrit = list(list(name = "maxiter", maxiter = 1000)),
               showpars = showpars, seed = 42)

 # 4: Plot output:
 plot(out1$Y[,1], out1$Y[,2], type = "p", pch = 20)
```

Have fun!  
Felipe
