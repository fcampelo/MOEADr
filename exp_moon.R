rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
library(stringr)
library(withr)
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)

repetitions <-  21

algorithms <- c("moead.de")

#uniform weight
resource.allocation.DRA <- list(name = "DRA", dt = 50)
resource.allocation.GRA <- list(name = "GRA", dt = 20)
resource.allocation.RAD <- list(name = "RAD", dt = 20)


decomp <- list(name = "SLD", H = 23)
decomp2 <- list(name = "uniform", N = 300)

# neighbors <- preset_moead("original")$neighbors
# neighbors$delta.p <- 0.9

scaling <- list()
scaling$name <- "simple"

variation <- preset_moead("moead.de")$variation
variation[[4]] <- variation[[3]]
variation[[3]] <-
  list(name = "localsearch",
       type = "dvls",
       gamma.ls = 0.5)

update <- preset_moead("moead.de")$update
update$UseArchive = TRUE
update$nsga = TRUE

update2 <- list(name  = "onra")
update2$UseArchive = TRUE
update2$nsga = TRUE

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
      epsilon   = 0.0) # tolerance for equality constraints
  )
  ref.points <- matrix(c(1.0, 0.0, 1.0), nrow = 1, ncol = 3)
  for (j in 1:4) {
    moead.de.data <- list()
    # moead.dra.data <- list()
    moead.gra.data <- list()
    moead.rad.data <- list()
    
    cat("rep:", j)
    
    
    # moead.de
    my.file.n <- paste0("de/",with_options(
      c(scipen = 999), 
      str_pad(j - 1, 3, pad = "0")
    ))
    # setwd(paste0("~/MOEADr/R/",my.file.n, "th_run"))
    moead.de <- moead(
      problem  = problem.zdt1,
      preset   = preset_moead(algo),
      decomp = decomp,
      stopcrit = stopcrit,
      scaling = scaling,
      update = update,
      constraint = list(name = "penalty", beta=0),
      showpars = list(show.iters = "none", showevery = 100),
      seed = j,
      my.file.n = my.file.n
    )
    # 
    # moead.dra <- moead(
    #   problem  = problem.zdt1,
    #   preset   = preset_moead(algo),
    #   decomp = decomp2,
    #   stopcrit = stopcrit,
    #   scaling = scaling,
    #   update = update,
    #   constraint = list(name = "penalty", beta=0),
    #   showpars = list(show.iters = "none", showevery = 100),
    #   seed = j,
    #   resource.allocation = resource.allocation.DRA
    # )
    # 
    # # gra.awt
    my.file.n <- paste0("gra/",with_options(
      c(scipen = 999), 
      str_pad(j - 1, 3, pad = "0")
    ))
    moead.gra <- moead(
      problem  = problem.zdt1,
      preset   = preset_moead(algo),
      decomp = decomp2,
      update = update2,
      stopcrit = stopcrit,
      scaling = scaling,
      constraint = list(name = "penalty", beta=0),
      showpars = list(show.iters = "none", showevery = 100),
      seed = j,
      resource.allocation = resource.allocation.GRA
    )
    # 
    # # ondb
    my.file.n <- paste0("rad/",with_options(
      c(scipen = 999), 
      str_pad(j - 1, 3, pad = "0")
    ))
    moead.rad <- moead(
      problem  = problem.zdt1,
      preset   = preset_moead(algo),
      decomp = decomp2,
      update = update2,
      stopcrit = stopcrit,
      scaling = scaling,
      constraint = list(name = "penalty", beta=0),
      showpars = list(show.iters = "none", showevery = 10),
      seed = j,
      resource.allocation = resource.allocation.RAD
    )
    # # scaling based on https://www.dora.dmu.ac.uk/xmlui/bitstream/handle/2086/15157/TR-CEC2018-MaOO-Competition.pdf?sequence=1
    # # Benchmark Functions for CEC’2018 Competition on Many-Objective Optimization
    # # instead of using nadir of pareto front using from estimation on the most basic algorithm tested
    # 
    # all.nadir <-
    #   rbind(moead.de$nadir,
    #         # moead.dra$nadir,
    #         moead.rad$nadir,
    #         moead.gra$nadir)
    # 
    # y.nadir <- moead.de$nadir
    # 
    # all.ideal <-
    #   rbind(moead.de$ideal,
    #         # moead.dra$ideal,
    #         moead.rad$ideal,
    #         moead.gra$ideal)
    # y.ideal <- moead.de$ideal
    # # for (i in 1:length(y.nadir)){
    # #   if (y.nadir[i]<0) y.nadir[i] <- 1e-50
    # # }
    # z <- 1:problem.zdt1$m
    # moead.rad$Y.norm <-
    #   unlist(sapply(z, function(i, apf, y.nadir, y.ideal) {
    #     (apf[, i] - y.ideal[i]) / (y.nadir[i] - y.ideal[i])
    #   }, apf = moead.rad$Y, y.nadir = y.nadir, y.ideal = y.ideal))
    # 
    # moead.gra$Y.norm <-
    #   unlist(sapply(z, function(i, apf, y.nadir) {
    #     (apf[, i] - y.ideal[i]) / (y.nadir[i] - y.ideal[i])
    #   }, apf = moead.gra$Y, y.nadir = y.nadir))
    # 
    # moead.de$Y.norm <-
    #   unlist(sapply(z, function(i, apf, y.nadir) {
    #     (apf[, i] - y.ideal[i]) / (y.nadir[i] - y.ideal[i])
    #   }, apf = moead.de$Archive$Y, y.nadir = y.nadir))
    # # 
    # # moead.dra$Y.norm <-
    # #   unlist(sapply(z, function(i, apf, y.nadir) {
    # #     (apf[, i] - y.ideal[i]) / (y.nadir[i] - y.ideal[i])
    # #   }, apf = moead.dra$Y, y.nadir = y.nadir))
    # 
    # # moead
    moead.de.non.d <- find_nondominated_points(moead.de$Archive$Y)
    moead.de.hv <-
      # dominated_hypervolume(t(as.matrix(dataTmp[, 1:nObj])), t(as.matrix(refPoint)))
      # emoa::dominated_hypervolume(points = t(moead.de$Y.norm[moead.de.non.d, ]),
      #                             ref = t(as.matrix(ref.points)))
      emoa::dominated_hypervolume(points = t(moead.de$Archive$Y[moead.de.non.d,]),
                                  ref = t(as.matrix(ref.points)))
    # 
    # moead.dra.non.d <- find_nondominated_points(moead.dra$Y.norm)
    # moead.dra.hv <-
    #   emoa::dominated_hypervolume(points = t(moead.dra$Y.norm[moead.dra.non.d, ]),
    #                               ref = ref.points)

    moead.rad.non.d <- find_nondominated_points(moead.rad$Archive$Y)
    moead.rad.hv <-
      emoa::dominated_hypervolume(points = t(moead.rad$Archive$Y[moead.rad.non.d, ]),
                                  ref = ref.points)

    # gra.awt
    moead.gra.non.d <- find_nondominated_points(moead.gra$Archive$Y)
    moead.gra.hv <-
      emoa::dominated_hypervolume(points = t(moead.gra$Archive$Y[moead.gra.non.d, ]),
                                  ref = ref.points)
    # 
    # 
    # 
    moead.de.data <-
      rbind(moead.de.data,
            moead.de.hv)
    # moead.dra.data <-
    #   rbind(moead.dra.data,
    #         moead.dra.hv)
    moead.gra.data <-
      rbind(moead.gra.data,
            moead.gra.hv)
    moead.rad.data <-
      rbind(moead.rad.data,
            moead.rad.hv)

    metrics <-
      rbind(
        unlist(moead.de.data),
        # unlist(moead.dra.data),
        unlist(moead.gra.data),
        unlist(moead.rad.data)
      )
    colnames(metrics) <- c("HV")
    names <-
      c('MOEA/D-DE',
        # 'MOEA/D-DRA',
        'MOEA/D-GRA',
        'MOEA/D-RAD')
    temp <- data.frame(metrics, algo, names, j)
    colnames(temp) <-
      c("HV",
        "base.algorithm",
        "variation.name",
        "rep.number_seed")
    if (exists("my.data")) {
      my.data <- rbind(my.data, temp)
    }
    else {
      my.data <- temp
    }
    print(aggregate(my.data$HV, mean, by = list(my.data$variation.name)))
  }
  print(my.data)
  save(my.data, file = "flymetothemoon")
}
