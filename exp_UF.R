rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(moobench)
library(MOEADr)
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)

algorithms <- c("moead.de")

increments <- list(lhs = NULL,
                   ONRA = list(onra = TRUE, dt = 20, ondb = NULL))

increments2 <- list(lhs = NULL,
                    ONRA = list(onra = NULL, dt = 20, ondb = TRUE))

decomp <- list(name = "SLD")

aggfun <- list(name = "AWT")

scaling <- list()
scaling$name <- "simple"

variation <- preset_moead("moead.de")$variation
variation[[4]] <- variation[[3]]
variation[[3]] <- list(name = "localsearch", type = "dvls", gamma.ls = 0.5)

update <- preset_moead("moead.de")$update
update$UseArchive = TRUE

update2 <- list(name  = "onra")
update2$UseArchive = TRUE

n.objs <- c(3)

stopcrit  <- list(list(name    = "maxeval",
                       maxiter = 30000))

for (n.obj in n.objs) {
  print(n.obj)
  if (n.obj == 2) {
    fun.names1 <- list()
    for (i in 1:7) {
      fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
    }
  }
  else if(n.obj == 3) {
    fun.names1 <- list()
    for (i in 8:10) {
      fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
    }
  }
  }
  
  my.data <- data.frame()
  for (algo in algorithms) {
    print(algo)
    if (algo == "moead.de") {
      number.instances <- 2
      num.iter.bet <- 3
    }
    else{
      number.instances <- 2
      num.iter.bet <- 3
    }
    f.num <- 0
    
    for (fun in fun.names1) {
      f.num <- f.num + 1
      print(fun)
        if (n.obj == 2) {
          decomp$H <- 199
        }
        if (n.obj == 3) {
          decomp$H <- 29
        }
        if (n.obj == 4) {
          decomp$H <- 9
        }
        d = 30
        problem <- load.UF.function(fun, n.obj)
        pareto.front <- problem$pf
        problem.uf <- problem$problem
        problem.zdt1 <- list(
          name       = "problem.moon",
          xmin       = rep(0, 2),
          xmax       = rep(1, 2),
          m          = 3
        )
      
      for (j in 1:21) {
        
        
        moead.de.data <- list()
        moead.de.ls.data <- list()
        onra.data <- list()
        gra.awt.data <- list()
        gra.awt.ls.data <- list()
        ondb.data <- list()
        ondb.ls.data <- list()
        
        cat("rep:", j)
        
        
        # # moead
        moead.de <- moead(
          problem  = problem.zdt1,
          preset   = preset_moead("moead.de"),
          decomp = decomp,
          stopcrit = stopcrit,
          constraint = list(name = "penalty"),
          moon.prob = TRUE,
          showpars = list(show.iters = "none", showevery = 100),
          seed = 42
        )
        
        # moead.de.ls <- moead(
        #   problem  = problem.zdt1,
        #   preset   = preset_moead(algo),
        #   decomp = decomp,
        #   stopcrit = stopcrit,
        #   update = update,
        #   scaling = scaling,
        #   variation = variation,
        #   showpars = list(show.iters = "none", showevery = 100),
        #   seed = j
        # )
        
        # onra
        onra <- moead(
          problem  = problem.zdt1,
          preset   = preset_moead(algo),
          decomp = decomp,
          increments = increments,
          # update = update,
          scaling = scaling,
          stopcrit = stopcrit,
          variation = variation,
          moon.prob = TRUE,
          showpars = list(show.iters = "none", showevery = 100),
          seed = j
        )
        
        # gra.awt
        gra.awt <- moead(
          problem  = problem.zdt1,
          preset   = preset_moead(algo),
          decomp = decomp,
          aggfun = aggfun,
          # update = update2,
          increments = increments,
          stopcrit = stopcrit,
          moon.prob = TRUE,
          # scaling = scaling,
          # variation = variation,
          showpars = list(show.iters = "none", showevery = 100),
          seed = j
        )
        
        # ondb
        ondb <- moead(
          problem  = problem.zdt1,
          preset   = preset_moead(algo),
          decomp = decomp,
          increments = increments2,
          stopcrit = stopcrit,
          moon.prob = TRUE,
          # variation = variation,
          # update = update,
          scaling = scaling,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j
        )
        # ondb.ls
        # ondb.ls <- moead(
        #   problem  = problem.zdt1,
        #   preset   = preset_moead(algo),
        #   decomp = decomp,
        #   increments = increments2,
        #   stopcrit = stopcrit,
        #   update = update,
        #   scaling = scaling,
        #   variation = variation,
        #   showpars = list(show.iters = "none", showevery = 100),
        #   seed = j
        # )
        
        # onra
        onra.IGD <- calcIGD(onra$Archive$Y, Yref = pareto.front)
        # gra.awt
        gra.awt.IGD <- calcIGD(gra.awt$Y, Yref = pareto.front)
        
        # ondb
        ondb.IGD <- calcIGD(ondb$Archive$Y, Yref = pareto.front)
        # ondb.ls
        # ondb.ls.IGD <- calcIGD(ondb.ls$Archive$Y, Yref = pareto.front)
        
        # moead
        moead.de.IGD <-
          calcIGD(moead.de$Y, Yref = pareto.front)
        # moead.ls
        # moead.de.ls.IGD <-
        # calcIGD(moead.de.ls$Archive$Y, Yref = pareto.front)
        
        
        # scal;ing based on https://www.dora.dmu.ac.uk/xmlui/bitstream/handle/2086/15157/TR-CEC2018-MaOO-Competition.pdf?sequence=1
        #Benchmark Functions for CEC’2018 Competition on Many-Objective Optimization
        y.nadir <- apply(pareto.front, 2, max)
        ondb$Y.norm <- ondb$Archive$Y/(1.1*y.nadir)
        # ondb.ls$Y.norm <- ondb$Archive$Y/(1.1*y.nadir)
        onra$Y.norm <- onra$Y/(1.1*y.nadir)
        gra.awt$Y.norm <- gra.awt$Y/(1.1*y.nadir)
        # moead.de.ls$Y.norm <- moead.de.ls$Archive$Y/(1.1*y.nadir)
        moead.de$Y.norm <- moead.de$Y/(1.1*y.nadir)
        ref.points <- rep(1+(1/decomp$H), problem.zdt1$m)
        
        # moead
        moead.de.non.d <- find_nondominated_points(moead.de$Y.norm)
        moead.de.hv <-
          emoa::dominated_hypervolume(points = t(moead.de$Y.norm[moead.de.non.d, ]),
                                      ref = ref.points)
        
        # moead.ls
        # moead.de.ls.non.d <- find_nondominated_points(moead.de.ls$Y.norm)
        # moead.de.ls.hv <-
        #   emoa::dominated_hypervolume(points = t(moead.de.ls$Y.norm[moead.de.ls.non.d, ]),
        #                               ref = ref.points)
        
        
        #onra
        onra.non.d <- find_nondominated_points(onra$Y.norm)
        onra.hv <-
          emoa::dominated_hypervolume(points = t(onra$Y.norm[onra.non.d, ]),
                                      ref = ref.points)
        
        # gra.awt
        gra.awt.non.d <- find_nondominated_points(gra.awt$Y.norm)
        gra.awt.hv <-
          emoa::dominated_hypervolume(points = t(gra.awt$Y.norm[gra.awt.non.d, ]),
                                      ref = ref.points)
        # ondb
        ondb.non.d <- find_nondominated_points(ondb$Y.norm)
        ondb.hv <-
          emoa::dominated_hypervolume(points = t(ondb$Y.norm[ondb.non.d, ]),
                                      ref = ref.points)
        # ondb
        # ondb.ls.non.d <- find_nondominated_points(ondb.ls$Y.norm)
        # ondb.ls.hv <-
        #   emoa::dominated_hypervolume(points = t(ondb.ls$Y.norm[ondb.ls.non.d, ]),
        #                               ref = ref.points)
        
        moead.de.data <-
          rbind(moead.de.data,
                moead.de.hv,
                moead.de.IGD)
        # moead.de.ls.data <-
        #   rbind(moead.de.ls.data,
        #         moead.de.ls.hv,
        #         moead.de.ls.IGD)
        rbind(onra.data,
              onra.hv,
              onra.IGD)
        gra.awt.data <-
          rbind(gra.awt.data,
                gra.awt.hv,
                gra.awt.IGD)
        onra.data <-
          rbind(onra.data,
                onra.hv,
                onra.IGD)
        ondb.data <-
          rbind(ondb.data,
                ondb.hv,
                ondb.IGD)
        # ondb.ls.data <-
        #   rbind(ondb.ls.data,
        #         ondb.ls.hv,
        #         ondb.ls.IGD)
        
        metrics <-
          rbind(
            unlist(moead.de.data),
            # unlist(moead.de.ls.data),
            unlist(onra.data),
            unlist(gra.awt.data),
            unlist(ondb.data)
            # unlist(ondb.ls.data)
          )
        colnames(metrics) <- c("HV", "IGD")
        names <-
          c(
            algo,
            # 'MOEA/D-DE.ls',
            'ONRA',
            'GRA.AWT',
            'ONDB'
            # 'ONDB.ls'
          )
        temp <- data.frame(metrics, fun, algo, names)
        colnames(temp) <- c("HV", "IGD", "function", "base.algorithm", "variation.name")
        if (exists("my.data")) {
          my.data <- rbind(my.data, temp)
        }
        else {
          my.data <- temp
        }
      }
      save(my.data, file = paste0(fun,"_",problem.zdt1$m))
    }
  }
}
