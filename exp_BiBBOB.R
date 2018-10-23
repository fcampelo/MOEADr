rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)

repetitions <-  21

algorithms <- c("moead.de")

increments <- list(lhs = NULL,
                   ONRA = list(onra = TRUE, dt = 20, ondb = NULL))

increments2 <- list(lhs = NULL,
                    ONRA = list(onra = NULL, dt = 20, ondb = TRUE))

decomp <- list(name = "SLD")
decomp$H <- 299
aggfun <- list(name = "AWT")

scaling <- list()
scaling$name <- "simple"

# variation <- preset_moead("moead.de")$variation
# variation[[4]] <- variation[[3]]
# variation[[3]] <-
#   list(name = "localsearch",
#        type = "dvls",
#        gamma.ls = 0.5)

# update <- preset_moead("moead.de")$update
# update$UseArchive = TRUE

update2 <- list(name  = "onra")
# update2$UseArchive = TRUE

n.objs <- c(2)

stopcrit  <- list(list(name    = "maxeval",
                       maxeval = 30000))

for (n.obj in n.objs) {
  print(n.obj)
  fun.names1 <- list()
  for (i in 1:55) {
    fun.names1[[length(fun.names1) + 1]] = paste0("BiObjBBOB", i)
  }
  
  my.data <- data.frame()
  for (algo in algorithms) {
    print(algo)
    j = 1
    for (fun in fun.names1) {
      print(fun)
      problem <-
        makeBiObjBBOBFunction(dimension = 20,
                              fid = j,
                              iid = 1)
      j = j + 1
      problem.BiBBOB <- function(X) {
        t(apply(X, MARGIN = 1,
                FUN = problem))
      }
      
      par.set = ParamHelpers::getParamSet(problem)
      problem.zdt1 <- list(
        name       = "problem.BiBBOB",
        xmin       = as.numeric(getLower(par.set)),
        xmax       = as.numeric(getUpper(par.set)),
        m          = n.obj
      )
      
      for (j in 1:repetitions) {
        moead.de.data <- list()
        # moead.de.ls.data <- list()
        # onra.data <- list()
        gra.awt.data <- list()
        # gra.awt.ls.data <- list()
        ondb.data <- list()
        nsga.2.data <- list()
        # ondb.ls.data <- list()
        
        cat("rep:", j)
        
        
        # # moead
        moead.de <- moead(
          problem  = problem.zdt1,
          preset   = preset_moead(algo),
          decomp = decomp,
          stopcrit = stopcrit,
          showpars = list(show.iters = "none", showevery = 100),
          seed = j
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
        # onra <- moead(
        #   problem  = problem.zdt1,
        #   preset   = preset_moead(algo),
        #   decomp = decomp,
        #   increments = increments,
        #   # update = update,
        #   scaling = scaling,
        #   stopcrit = stopcrit,
        #   # variation = variation,
        #   showpars = list(show.iters = "none", showevery = 100),
        #   seed = j
        # )
        
        # gra.awt
        gra.awt <- moead(
          problem  = problem.zdt1,
          preset   = preset_moead(algo),
          decomp = decomp,
          aggfun = aggfun,
          # update = update2,
          increments = increments,
          stopcrit = stopcrit,
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
        
        par.set = ParamHelpers::getParamSet(problem)
        nsga.2 = mco::nsga2(
          problem,
          idim = getNumberOfParameters(problem),
          odim = n.obj,
          lower.bounds = as.numeric(getLower(par.set)),
          upper.bounds = as.numeric(getUpper(par.set)),
          popsize = 300
        )
        
        
        
        # scaling based on https://www.dora.dmu.ac.uk/xmlui/bitstream/handle/2086/15157/TR-CEC2018-MaOO-Competition.pdf?sequence=1
        # Benchmark Functions for CEC’2018 Competition on Many-Objective Optimization
        # instead of using nadir of pareto front using from estimation on the most basic algorithm tested
        feas.idx <- rep(TRUE, nrow(moead.de$Y))
        y.nadir <- apply(moead.de$Y[feas.idx, ], 2, max)
        ondb$Y.norm <- ondb$Y / (1.1 * y.nadir)
        # ondb.ls$Y.norm <- ondb$Archive$Y/(1.1*y.nadir)
        # onra$Y.norm <- onra$Y / (1.1 * y.nadir)
        gra.awt$Y.norm <- gra.awt$Y / (1.1 * y.nadir)
        # moead.de.ls$Y.norm <- moead.de.ls$Archive$Y/(1.1*y.nadir)
        moead.de$Y.norm <- moead.de$Y / (1.1 * y.nadir)
        nsga.2$Y.norm <- nsga.2$value / (1.1 * y.nadir)
        ref.points <- rep(1 + (1 / decomp$H), problem.zdt1$m)
        
        # moead
        moead.de.non.d <- find_nondominated_points(moead.de$Y.norm)
        moead.de.hv <-
          emoa::dominated_hypervolume(points = t(moead.de$Y.norm[moead.de.non.d,]),
                                      ref = ref.points)
        
        # moead.ls
        # moead.de.ls.non.d <- find_nondominated_points(moead.de.ls$Y.norm)
        # moead.de.ls.hv <-
        #   emoa::dominated_hypervolume(points = t(moead.de.ls$Y.norm[moead.de.ls.non.d, ]),
        #                               ref = ref.points)
        
        
        # #onra
        # onra.non.d <- find_nondominated_points(onra$Y.norm)
        # onra.hv <-
        #   emoa::dominated_hypervolume(points = t(onra$Y.norm[onra.non.d,]),
                                      # ref = ref.points)
        
        # gra.awt
        gra.awt.non.d <- find_nondominated_points(gra.awt$Y.norm)
        gra.awt.hv <-
          emoa::dominated_hypervolume(points = t(gra.awt$Y.norm[gra.awt.non.d,]),
                                      ref = ref.points)
        # ondb
        ondb.non.d <- find_nondominated_points(ondb$Y.norm)
        ondb.hv <-
          emoa::dominated_hypervolume(points = t(ondb$Y.norm[ondb.non.d,]),
                                      ref = ref.points)
        # ondb
        # ondb.ls.non.d <- find_nondominated_points(ondb.ls$Y.norm)
        # ondb.ls.hv <-
        #   emoa::dominated_hypervolume(points = t(ondb.ls$Y.norm[ondb.ls.non.d, ]),
        #                               ref = ref.points)
        
        nsga.2.non.d <- find_nondominated_points(nsga.2$Y.norm)
        nsga.2.hv <-
          emoa::dominated_hypervolume(points = t(nsga.2$Y.norm[nsga.2.non.d,]),
                                      ref = ref.points)
        
        
        moead.de.data <-
          rbind(moead.de.data,
                moead.de.hv)#,
        # moead.de.IGD)
        # moead.de.ls.data <-
        #   rbind(moead.de.ls.data,
        #         moead.de.ls.hv,
        #         moead.de.ls.IGD)
        # onra.data <-
        #   rbind(onra.data,
        #         onra.hv)#,
        # onra.IGD)
        #gra.awt
        gra.awt.data <-
          rbind(gra.awt.data,
                gra.awt.hv)#,
        # gra.awt.IGD)
        # onra.IGD)
        ondb.data <-
          rbind(ondb.data,
                ondb.hv)#,
        # ondb.IGD)
        # ondb.ls.data <-
        #   rbind(ondb.ls.data,
        #         ondb.ls.hv,
        #         ondb.ls.IGD)
        nsga.2.data <-
          rbind(nsga.2.data,
                nsga.2.hv)
        
        metrics <-
          rbind(
            unlist(moead.de.data),
            # unlist(moead.de.ls.data),
            # unlist(onra.data),
            unlist(gra.awt.data),
            unlist(ondb.data),
            unlist(nsga.2.data)
            # unlist(ondb.ls.data)
          )
        colnames(metrics) <- c("HV")
        names <-
          c(algo,
            # 'MOEA/D-DE.ls',
            # 'ONRA',
            'GRA.AWT',
            'ONDB',
            'NSGA-2')
            # 'ONDB.ls')
            temp <- data.frame(metrics, fun, algo, names)
            colnames(temp) <-
              c("HV",
                "function",
                "base.algorithm",
                "variation.name")
            if (exists("my.data")) {
              my.data <- rbind(my.data, temp)
            }
            else {
              my.data <- temp
            }
      }
      save(my.data, file = paste0(fun, "_", problem.zdt1$m))
    }
  }
}
