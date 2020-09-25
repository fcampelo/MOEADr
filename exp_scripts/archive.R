rm(list = ls(all = TRUE))
# setwd("~/MOEADr/R/")
library(smoof)
# library(MOEADr)
library(emoa)
library(feather)
# library(withr)
library(compiler)
library(ggplot2)
# source("~/MOEADr/R/utils.R")
# lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)
enableJIT(1)

library(MOEADps)


source("~/MOEADr/R/savePlotData.R")
# source("~/MOEADr/R/moead.R")
source("~/MOEADr/R/utils.R")

repetitions <-  10
dimension <- 100


n.obj <- 2
lambda <- 40
decomp <- list(name = "sld", H = 49, .nobj = n.obj)
decomp2 <- list(name = "sld", H = 499, .nobj = n.obj)
loaded.weights.2objs <-
  data.matrix(
    read.csv(
      "~/MOEADr/SOBOL-2objs-500wei.ws",
      header = F,
      stringsAsFactors = FALSE,
      sep = " "
    )
  )
decomp.loaded.2 <- list(name = "loaded", W = loaded.weights.2objs)

variation = preset_moead("moead.de")$variation
variation[[2]]$pm = 1 / dimension
scaling <- list()
scaling$name <- "simple"
neighbors <- preset_moead("moead.de")$neighbors
neighbors$T <- 100
neighbors3 <- preset_moead("moead.de")$neighbors
neighbors3$T <- 10
update <- preset_moead("moead.de")$update
update$UseArchive = TRUE

stopcrit  <- list(list(name    = "maxeval",
                       maxeval = 100000))







print("2 OBJECTIVES")
problem.to.solve <- c("DTLZ3")
for (fun in problem.to.solve) {
  problem <-
    load.DTLZ.function(fun, dimension = dimension, n.obj = n.obj)
  problem.smoof.DTLZ <- problem$fn
  problem.DTLZ <- function(X) {
    t(-apply(X, MARGIN = 1,
            FUN = problem.smoof.DTLZ))
  }
  par.set = ParamHelpers::getParamSet(problem.smoof.DTLZ)
  problem.dtlz7 <- list(
    name       = "problem.DTLZ",
    xmin       = as.numeric(getLower(par.set)),
    xmax       = as.numeric(getUpper(par.set)),
    m          = n.obj
  )
  for (j in 1:repetitions) {
    cat("rep", j, "\n")
    
    resource.allocation.RANDOM <-
      list(
        name = "random",
        dt = 0,
        selection = "n",
        n = lambda
      )
    
    # update$nr <- 1
    # moead.smallT <- moead(
    #   problem  = problem.dtlz7,
    #   preset   = preset_moead("moead.de"),
    #   decomp = decomp.loaded.2,
    #   variation = variation,
    #   stopcrit = stopcrit,
    #   scaling = scaling,
    #   neighbors = neighbors,
    #   showpars = list(show.iters = "none", showevery = 1000),
    #   seed = j + 422,
    #   update = update,
    #   resource.allocation = resource.allocation.RANDOM,
    #   small.update = TRUE
    # )
    # 
    # savePlotData(
    #   moea = moead.smallT,
    #   name = paste0(fun, "_moead.smallT_", lambda, "_"),
    #   j = j,
    #   wd = "~/jsec_2020_50/"
    # )
    # rm(moead.smallT)
    
    # update$nr <- 2
    moead.random.500 <- moead(
      problem  = problem.dtlz7,
      preset   = preset_moead("moead.de"),
      decomp = decomp2,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors,
      showpars = list(show.iters = "none", showevery = 1000),
      seed = j + 4212,
      update = update,
      resource.allocation = resource.allocation.RANDOM
    )
    
    
    moead.random.3 <- moead(
      problem  = problem.dtlz7,
      preset   = preset_moead("moead.de"),
      decomp = decomp2,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors3,
      showpars = list(show.iters = "none", showevery = 1000),
      seed = j + 4212,
      update = update,
      resource.allocation = resource.allocation.RANDOM
    )
    
    # savePlotData(
    #   moea = moead.random,
    #   name = paste0(fun, "_moead.random_", lambda, "_"),
    #   j = j,
    #   wd = "~/jsec_2020_50/"
    # )
    # rm(moead.random)
    
    moead.3 <- moead(
      problem  = problem.dtlz7,
      preset   = preset_moead("moead.de"),
      decomp = decomp,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors3,
      showpars = list(show.iters = "none", showevery = 1000),
      seed = j + 4212,
      update = update
    )
    
    # savePlotData(
    #   moea = moead.3,
    #   name = paste0(fun, "_moead.3_", lambda, "_"),
    #   j = j,
    #   wd = "~/jsec_2020_50/"
    # )
    # rm(moead.3)
    
    
    moead500 <- moead(
      problem  = problem.dtlz7,
      preset   = preset_moead("moead.de"),
      decomp = decomp2,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors,
      showpars = list(show.iters = "none", showevery = 1000),
      seed = j + 4212,
      update = update
    )
    
    # savePlotData(
    #   moea = moead500,
    #   name = paste0(fun, "_moead500_", lambda, "_"),
    #   j = j,
    #   wd = "~/jsec_2020_50/"
    # )
    # rm(moead500)
    
    Yref <-
      as.matrix(read.table(paste0(
        "~/MOEADr/inst/extdata/pf_data/", fun, ".3D.pf"
      )))
    
    colnames(Yref) <- c("f1", "f2")
    plot.data3 <-
      rbind(
        data.frame(moead.3$Archive$Y[ecr::nondominated(t(moead.3$Archive$Y)),], Strategy = "Small pop"),
        data.frame(moead500$Archive$Y[ecr::nondominated(t(moead500$Archive$Y)),], Strategy = "Big pop"),
        data.frame(
          moead.random.500$Archive$Y[ecr::nondominated(t(moead.random.500$Archive$Y)),],
          Strategy = paste0("MOEA/D PS=", lambda, "pop=",dim(moead.random.500$W)[1])
        ),
        data.frame(Yref, Strategy = "Ref Front")
        # data.frame(Yref, Strategy = "MOEA/D PS=5, pop=50")
      )
    plot <- ggplot(plot.data3, aes(f1, f2, size = 18)) +
      geom_point(aes(color = Strategy, shape = Strategy), alpha = 0.5) +
      theme_minimal(base_size = 26) #+ scale_colour_hc()
    plot <- plot +  theme(axis.text = element_text(size =24), legend.background = element_rect(size=0.5, linetype="solid"))
    print(plot + theme(legend.position = "bottom", legend.title = element_blank())+guides(size = FALSE, shape = guide_legend(override.aes = list(size = 5))
    ))
    filename = paste0("~/jsec_2020_50/PFs/playing",fun,".eps")
    ggsave(
      filename = filename,
      dpi = 1200,
      width = 12,
      height = 12
    )
    
    
    ref1 <- rbind(moead.3$Archive$Y, moead500$Archive$Y, moead.random.3$Archive$Y, moead.random.500$Archive$Y)
    ref.point <- rep(1, n.obj)
    moead.3$scaledY <- scaling_Y(moead.3$Archive$Y[ecr::nondominated(t(moead.3$Archive$Y)), ], ref1)
    dominated_hypervolume(t(moead.3$scaledY), ref = ref.point)
    
    moead500$scaledY <- scaling_Y(moead500$Archive$Y[ecr::nondominated(t(moead500$Archive$Y)), ], ref1)
    dominated_hypervolume(t(moead500$scaledY), ref = ref.point)
    
    moead.random.500$scaledY <- scaling_Y(moead.random.500$Archive$Y[ecr::nondominated(t(moead.random.500$Archive$Y)), ], ref1)
    dominated_hypervolume(t(moead.random.500$scaledY), ref = ref.point)
    
    # moead.random.3$scaledY <- scaling_Y(moead.random.3$Archive$Y[ecr::nondominated(t(moead.random.3$Archive$Y)), ], ref1)
    # dominated_hypervolume(t(moead.random.3$scaledY), ref = ref.point)
    
    
    sum(ecr::nondominated(t(moead.3$Archive$Y[ecr::nondominated(t(moead.3$Archive$Y)), ])))
    sum(ecr::nondominated(t(moead500$Archive$Y[ecr::nondominated(t(moead500$Archive$Y)), ])))
    sum(ecr::nondominated(t(moead.random.500$Archive$Y[ecr::nondominated(t(moead.random.500$Archive$Y)), ])))
    # sum(ecr::nondominated(t(moead.random.3$Archive$Y[ecr::nondominated(t(moead.random.3$Archive$Y)), ])))
    
    
    
    
    
    exit()
  }
}


