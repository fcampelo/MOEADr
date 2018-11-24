# rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
library(ecr)
library(stringr)
library(withr)
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)

repetitions <-  30

algorithms <- c("moead.de")

#uniform weight
resource.allocation.DRA <- list(name = "DRA", dt = 50)
resource.allocation.GRA <- list(name = "GRA", dt = 20)
resource.allocation.RAD <- list(name = "RAD", dt = 20)



decomp <- list(name = "SLD", H = 16)

preset <- preset_moead("moead.de")
preset$decomp <- decomp
preset$aggfun$name <- "awt"
preset$neighbors$name <- "lambda"
preset$neighbors$T <- 11
preset$neighbors$delta.p <- 0.909
preset$variation[[1]]$phi   <- NULL
preset$variation[[2]]$pm   <- 0.445
preset$variation[[4]] <- list(name = "localsearch", type = "tpqa", gamma.ls = 0.95)

update2 <- preset_moead("moead.de")$update
update2$UseArchive = TRUE
update2$nsga = TRUE
update2$ws_transformation = FALSE

update <- preset_moead("moead.de")$update
update$UseArchive = TRUE
update$nsga = TRUE
update$ws_transformation = TRUE

preset$update <- update


variation <- preset_moead("moead.de")$variation
variation[[4]] <- list(name = "localsearch", type = "tpqa", gamma.ls = 0.95)



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
      name      = "unitary_constraints",# constraint function routine
      epsilon   = 0.95) # tolerance for equality constraints
  )
  ref.points <- matrix(c(1.0, 0.0, 1.0), nrow = 1, ncol = 3)
  for (j in 1:repetitions) {
    
    # cat("rep:", j)
    
    
    # moead.de
    #print("moead.de")
    # my.file.n <- paste0("de/",with_options(
    #  c(scipen = 999),
    #  str_pad(j - 1, 3, pad = "0")
    # ))
    # moead.de <- moead(
    #   problem  = problem.zdt1,
    #   preset   = preset_moead(algo),
    #   decomp = decomp,
    #   stopcrit = stopcrit,
    #   update = update2,
    #   variation = variation,
    #   constraint = list(name = "penalty", beta=0.95),
    #   showpars = list(show.iters = "none", showevery = 100),
    #   seed = j,
    #   my.file.n = my.file.n
    # )
    # my.file.n <- paste0("gra/",with_options(
    #   c(scipen = 999),
    #   str_pad(j - 1, 3, pad = "0")
    # ))
    # moead.de <- moead(
    #  problem  = problem.zdt1,
    #  preset   = preset,
    #  stopcrit = stopcrit,
    #  constraint = list(name = "penalty", beta=0.95),
    #  showpars = list(show.iters = "none", showevery = 100),
    #  seed = j,
    #  my.file.n = my.file.n
    # )
    # 
    # # # ondb
    my.file.n <- paste0("rad/",with_options(
      c(scipen = 999),
      str_pad(j - 1, 3, pad = "0")
    ))
    # print("moead.rad")
    moead.rad <- moead(
      problem  = problem.zdt1,
      preset   = preset,
      stopcrit = stopcrit,
      constraint = list(name = "penalty", beta=0.95),
      showpars = list(show.iters = "none", showevery = 100),
      seed = j,
      resource.allocation = resource.allocation.RAD,
      my.file.n = my.file.n
    )
    print("moead.rad$n.iter")
    print(moead.rad$n.iter)
    
    
    # moead.de.non.d <- find_nondominated_points(moead.de$Archive2$Y)
    # moead.de.hv <-
    #   emoa::dominated_hypervolume(points = t(moead.de$Archive2$Y),
    #                               ref = ref.points)
    # print(moead.de.hv)
    # moead.rad.non.d <- find_nondominated_points(moead.rad$Archive2$Y)
    # moead.rad.hv <-
    #   emoa::dominated_hypervolume(points = t(moead.rad$Archive2$Y),
    #                               ref = ref.points)
    # print(moead.rad.hv)
  
  }
}
