rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
library(ecr)
library(stringr)
library(withr)
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)

repetitions <-  21

algorithms <- c("moead.de")

#uniform weight
resource.allocation.DRA <- list(name = "DRA", dt = 50)
resource.allocation.GRA <- list(name = "GRA", dt = 20)
resource.allocation.RAD <- list(name = "RAD", dt = 20)


decomp <- list(name = "SLD", H = 16)
decomp2 <- list(name = "uniform", N = 150)

scaling <- list()
scaling$name <- "simple"

variation <- preset_moead("moead.de")$variation
# variation[[4]] <- variation[[3]]
variation[[1]] <-
  list(name = "de")

update <- preset_moead("moead.de")$update
update$UseArchive = TRUE
update$nsga = FALSE


n.obj <- 3


stopcrit  <- list(list(name    = "maxeval",
                       maxeval = 30000))

d <- 2

my.data <- data.frame()
for (algo in algorithms) {
  print(algo)
  problem.zdt1 <- list(
    name       = "problem.moon",
    xmin       = rep(0, d),
    xmax       = rep(1, d),
    m          = n.obj,
    constraints = list(
      name      = "box_constraints",# constraint function routine
      epsilon   = 0.00) # tolerance for equality constraints
  )
  ref.points <- matrix(c(1.0, 0.0, 1.0), nrow = 1, ncol = 3)
  for (j in 1:repetitions) {
    # moead.de.data <- list()
    # # moead.dra.data <- list()
    # moead.gra.data <- list()
    # moead.rad.data <- list()
    
    cat("rep:", j)
    
    
    # moead.de
    my.file.n <- paste0("de/",with_options(
      c(scipen = 999),
      str_pad(j - 1, 3, pad = "0")
    ))
    moead.de <- moead(
      problem  = problem.zdt1,
      preset   = preset_moead(algo),
      decomp = decomp,
      stopcrit = stopcrit,
      variation = variation,
      constraint = list(name = "penalty", beta=0.05),
      showpars = list(show.iters = "none", showevery = 100),
      seed = j,
      my.file.n = my.file.n
    )
    # # #
    # # # # gra.awt
    # my.file.n <- paste0("gra/",with_options(
    #   c(scipen = 999),
    #   str_pad(j - 1, 3, pad = "0")
    # ))
    # moead.gra <- moead(
    #   problem  = problem.zdt1,
    #   preset   = preset_moead(algo),
    #   decomp = decomp,
    #   update = update,
    #   stopcrit = stopcrit,
    #   constraint = list(name = "penalty", beta=0.0),
    #   showpars = list(show.iters = "none", showevery = 100),
    #   seed = j,
    #   my.file.n = my.file.n
    # )

    # # ondb
    my.file.n <- paste0("rad/",with_options(
      c(scipen = 999), 
      str_pad(j - 1, 3, pad = "0")
    ))
    moead.rad <- moead(
      problem  = problem.zdt1,
      preset   = preset_moead(algo),
      decomp = decomp,
      stopcrit = stopcrit,
      # update = update,
      variation = variation,
      constraint = list(name = "penalty", beta=0.0),
      showpars = list(show.iters = "none", showevery = 10),
      seed = j,
      resource.allocation = resource.allocation.RAD,
      my.file.n = my.file.n
    )
    print("moead.rad$n.iter")
    print(moead.rad$n.iter)
    # moead.rad.non.d <- find_nondominated_points(moead.rad$Y)
    # moead.rad.hv <-
    #   emoa::dominated_hypervolume(points = t(moead.rad$Y[moead.rad.non.d, ]),
    #                               ref = ref.points)
    # print(moead.rad.hv)
    # print("moead.de")
    # # print(moead.de$n.iter)
    # moead.de.non.d <- find_nondominated_points(moead.de$Y)
    # moead.de.hv <-
    #   emoa::dominated_hypervolume(points = t(moead.de$Y[moead.de.non.d, ]),
    #                               ref = ref.points)
    # print(moead.de.hv)
    # moead.de.non.d <- find_nondominated_points(moead.gra$Y)
    # moead.de.hv <-
    #   emoa::dominated_hypervolume(points = t(moead.gra$Y[moead.de.non.d, ]),
    #                               ref = ref.points)
    # print(moead.de.hv)
  }
}
