rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
library(stringr)
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)

repetitions <-  21

algorithms <- c("moead.de")

#uniform weight
# resource.allocation.DRA <- list(name = "DRA", dt = 50)
resource.allocation.GRA <- list(name = "GRA", dt = 20)
resource.allocation.RAD <- list(name = "RAD", dt = 20)


decomp <- list(name = "SLD", H = 149)
decomp2 <- list(name = "uniform", N = 150)

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

update2 <- list(name  = "onra")
# update2$UseArchive = TRUE

n.objs <- c(2)


stopcrit  <- list(list(name    = "maxeval",
                       maxeval = 30000))

for (n.obj in n.objs) {
  print(n.obj)
  fun.names1 <- list()
  for (i in 11:11) {
    fun.names1[[length(fun.names1) + 1]] = paste0("BiObjBBOB", i)
  }
  for (i in 13:13) {
    fun.names1[[length(fun.names1) + 1]] = paste0("BiObjBBOB", i)
  }
  
  my.data <- data.frame()
  for (algo in algorithms) {
    print(algo)
    id = 1
    for (fun in fun.names1) {
      print(fun)
      problem <-
        makeBiObjBBOBFunction(dimension = 20,
                              fid = id,
                              iid = 1)
      id = id + 1
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
      ref.points <- rep(1, problem.zdt1$m)
      
      for (j in 1:repetitions) {
        moead.de.data <- list()
        # moead.dra.data <- list()
        moead.gra.data <- list()
        moead.rad.data <- list()
        # nsga.2.data <- list()
        
        
        cat("rep:", j)
        
        
        # # moead.de
        moead.de <- moead(
          problem  = problem.zdt1,
          preset   = preset_moead(algo),
          decomp = decomp,
          stopcrit = stopcrit,
          # scaling = scaling,
          showpars = list(show.iters = "none", showevery = 100),
          seed = j
        )
        
        # moead.dra <- moead(
        #   problem  = problem.zdt1,
        #   preset   = preset_moead(algo),
        #   decomp = decomp2,
        #   stopcrit = stopcrit,
        #   scaling = scaling,
        #   showpars = list(show.iters = "none", showevery = 100),
        #   seed = j,
        #   resource.allocation = resource.allocation.DRA
        # )
        
        # gra.awt
        moead.gra <- moead(
          problem  = problem.zdt1,
          preset   = preset_moead(algo),
          decomp = decomp2,
          update = update2,
          stopcrit = stopcrit,
          # scaling = scaling,
          showpars = list(show.iters = "none", showevery = 100),
          seed = j,
          resource.allocation = resource.allocation.GRA
        )
        
        # ondb
        moead.rad <- moead(
          problem  = problem.zdt1,
          preset   = preset_moead(algo),
          decomp = decomp2,
          update = update2,
          stopcrit = stopcrit,
          # scaling = scaling,
          showpars = list(show.iters = "none", showevery = 10),
          seed = j,
          
          resource.allocation = resource.allocation.RAD
        )
        
        # par.set = ParamHelpers::getParamSet(problem)
        # nsga.2 = mco::nsga2(
        #   problem,
        #   idim = getNumberOfParameters(problem),
        #   generations = moead.de$n.iter,
        #   odim = n.obj,
        #   lower.bounds = as.numeric(getLower(par.set)),
        #   upper.bounds = as.numeric(getUpper(par.set)),
        #   popsize = (floor(dim(moead.de$W)[1]/4))*4
        # )
        
        
        # scaling based on https://www.dora.dmu.ac.uk/xmlui/bitstream/handle/2086/15157/TR-CEC2018-MaOO-Competition.pdf?sequence=1
        # Benchmark Functions for CEC’2018 Competition on Many-Objective Optimization
        # instead of using nadir of pareto front using from estimation on the most basic algorithm tested
        
        # nsga.2$nadir <- apply(nsga.2$value, 2, max)
        # nsga.2$ideal <- apply(nsga.2$value, 2, min)
        # 
        all.nadir <-
          rbind(moead.de$nadir,
                # moead.dra$nadir,
                moead.rad$nadir,
                moead.gra$nadir)#,
                # nsga.2$nadir)
        
        y.nadir <- apply(all.nadir, 2, max)
        
        all.ideal <-
          rbind(moead.de$ideal,
                # moead.dra$ideal,
                moead.rad$ideal,
                moead.gra$ideal)#,
                # nsga.2$ideal)
        y.ideal <- apply(all.ideal, 2, min)
        # for (i in 1:length(y.nadir)){
        #   if (y.nadir[i]<0) y.nadir[i] <- 1e-50
        # }
        z <- 1:problem.zdt1$m
        moead.rad$Y.norm <-
          unlist(sapply(z, function(i, apf, y.nadir, y.ideal) {
            (apf[, i] - y.ideal[i]) / (y.nadir[i] - y.ideal[i])
          }, apf = moead.rad$Y, y.nadir = y.nadir, y.ideal = y.ideal))
        
        moead.gra$Y.norm <-
          unlist(sapply(z, function(i, apf, y.nadir) {
            (apf[, i] - y.ideal[i]) / (y.nadir[i] - y.ideal[i])
          }, apf = moead.gra$Y, y.nadir = y.nadir))
        
        moead.de$Y.norm <-
          unlist(sapply(z, function(i, apf, y.nadir) {
            (apf[, i] - y.ideal[i]) / (y.nadir[i] - y.ideal[i])
          }, apf = moead.de$Y, y.nadir = y.nadir))
        # 
        # moead.dra$Y.norm <-
        #   unlist(sapply(z, function(i, apf, y.nadir) {
        #     (apf[, i] - y.ideal[i]) / (y.nadir[i] - y.ideal[i])
        #   }, apf = moead.dra$Y, y.nadir = y.nadir))
        # 
        # nsga.2$Y.norm <-
        #   unlist(sapply(z, function(i, apf, y.nadir) {
        #     (apf[, i] - y.ideal[i]) / (y.nadir[i] - y.ideal[i])
        #   }, apf = nsga.2$value, y.nadir = y.nadir))
        # 
        # moead
        moead.de.non.d <- find_nondominated_points(moead.de$Y.norm)
        moead.de.hv <-
          emoa::dominated_hypervolume(points = t(moead.de$Y.norm[moead.de.non.d, ]),
                                      ref = ref.points)
        
        # moead.dra.non.d <- find_nondominated_points(moead.dra$Y.norm)
        # moead.dra.hv <-
        #   emoa::dominated_hypervolume(points = t(moead.dra$Y.norm[moead.dra.non.d, ]),
        #                               ref = ref.points)
        
        moead.rad.non.d <- find_nondominated_points(moead.rad$Y.norm)
        moead.rad.hv <-
          emoa::dominated_hypervolume(points = t(moead.rad$Y.norm[moead.rad.non.d, ]),
                                      ref = ref.points)

        # gra.awt
        moead.gra.non.d <- find_nondominated_points(moead.gra$Y.norm)
        moead.gra.hv <-
          emoa::dominated_hypervolume(points = t(moead.gra$Y.norm[moead.gra.non.d, ]),
                                      ref = ref.points)
        # 
        # nsga.2.non.d <- find_nondominated_points(nsga.2$Y.norm)
        # nsga.2.hv <-
        #   emoa::dominated_hypervolume(points = t(nsga.2$Y.norm[nsga.2.non.d, ]),
        #                               ref = ref.points)
        
        
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
        # nsga.2.data <-
        #   rbind(nsga.2.data,
        #         nsga.2.hv)
        
        metrics <-
          rbind(
            unlist(moead.de.data),
            # unlist(moead.dra.data),
            unlist(moead.gra.data),
            unlist(moead.rad.data)#,
            # unlist(nsga.2.data)
          )
        colnames(metrics) <- c("HV")
        names <-
          c('MOEA/D-DE',
            # 'MOEA/D-DRA',
            'MOEA/D-GRA',
            'MOEA/D-RAD')#,
            # 'NSGA-2')
        temp <- data.frame(metrics, fun, algo, names)
        colnames(temp) <-
          c("HV",
            "fun",
            "base.algorithm",
            "variation.name")
        if (exists("my.data")) {
          my.data <- rbind(my.data, temp)
        }
        else {
          my.data <- temp
        }
        print(aggregate(my.data$HV, median, by = list(my.data$variation.name, my.data$fun)))
      }
      save(my.data, file = paste0(fun, "_", problem.zdt1$m))
    }
  }
}
