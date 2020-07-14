rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
library(feather)
library(withr)
library(compiler)
library(gridExtra)
# exit()
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)
#exit()
library("Kendall")
repetitions <-  1
dimension <- 100

data.frame.metrics <- function(moea) {
  data.metrics <- rbind(
  #   data.frame(
  #   value = (unlist(moea$p.fet)),
  #   metric = "p.fet",
  #   iteration = 1:length(unlist(moea$p.fet))
  # ),
  data.frame(
    value = moea$pressure.offspring,
    metric = "p.tau",
    iteration = 1:length(moea$pressure.offspring)
  )
  # data.frame(
  #   value = (unlist(moea$spread)),
  #   metric = "spread",
  #   iteration = 1:length(unlist(moea$spread))
  # )
  )
  return (data.metrics)
}

fun.names1 <- list()
for (i in 4:7) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}

n.obj = 2

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


update.restricted <- preset_moead("moead.de")$update
update.restricted$UseArchive = TRUE

update.best <- preset_moead("moead.de")$update
update.best$name <- "best"
update.best$Tr <- 500
update.best$nr <- 500
update.best$UseArchive = TRUE

stopcrit  <- list(list(name    = "maxeval",
                       maxeval = 100000))

decomp2 <- list(name = "diverge", H = 499, .nobj = 2)
decomp <- list(name = "sld", H = 99, .nobj = 2)

aggfun <- preset_moead("moead.de")$aggfun
aggfun$name <- "diverge"

resource.allocation.RANDOM_FIXED.0.1 <-
  list(
    name = "random_fixed",
    dt = 0,
    # heads up
    selection = "n",
    n = 3,
    # in case we want to move from stochastic
    fixed_value = 0.1
  )

resource.allocation.RANDOM_FIXED.50 <-
  list(
    name = "random_fixed",
    dt = 0,
    # heads up
    selection = "n",
    n = 50,
    # in case we want to move from stochastic
    fixed_value = 0.1
  )

resource.allocation.RANDOM_FIXED.100 <-
  list(
    name = "random_fixed",
    dt = 0,
    # heads up
    selection = "n",
    n = 100,
    # in case we want to move from stochastic
    fixed_value = 0.1
  )

for (j in 1:repetitions) {
  for (fun in fun.names1) {
    problem <-
      load.DTLZ.function(fun, dimension = dimension, n.obj = n.obj)
    problem.smoof.DTLZ <- problem$fn
    problem.DTLZ <- function(X) {
      t(apply(X, MARGIN = 1,
              FUN = problem.smoof.DTLZ))
    }
    par.set = ParamHelpers::getParamSet(problem.smoof.DTLZ)
    problem.DTLZ2 <- list(
      name       = "problem.DTLZ",
      xmin       = as.numeric(getLower(par.set)),
      xmax       = as.numeric(getUpper(par.set)),
      m          = n.obj
    )
    
    
    Yref <-
      as.matrix(read.table(paste0(
        "../inst/extdata/pf_data/", fun, ".2D.pf"
      )))
    
    colnames(Yref) <- c("f1", "f2")
    
    
    seed = sample(1:10000)[1]
    moead.de <- moead(
      problem  = problem.DTLZ2,
      preset   = preset_moead("moead.de"),
      decomp = decomp.loaded.2,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors,
      # aggfun = aggfun,
      showpars = list(show.iters = "none", showevery = 10),
      seed = seed,
      update = update.restricted
    )
    
    moead.de.diverge <- moead(
      problem  = problem.DTLZ2,
      preset   = preset_moead("moead.de"),
      decomp = decomp2,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors,
      aggfun = aggfun,
      showpars = list(show.iters = "none", showevery = 10),
      seed = seed,
      update = update.restricted
    )
    
    moead.random.3 <- moead(
      problem  = problem.DTLZ2,
      preset   = preset_moead("moead.de"),
      decomp = decomp.loaded.2,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors,
      showpars = list(show.iters = "none", showevery = 10),
      seed = seed,
      update = update.restricted,
      resource.allocation = resource.allocation.RANDOM_FIXED.0.1,
    )
    
    moead.random.50 <- moead(
      problem  = problem.DTLZ2,
      preset   = preset_moead("moead.de"),
      decomp = decomp.loaded.2,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors,
      showpars = list(show.iters = "none", showevery = 10),
      seed = seed,
      update = update.restricted,
      resource.allocation = resource.allocation.RANDOM_FIXED.50,
    )

    moead.random.100 <- moead(
      problem  = problem.DTLZ2,
      preset   = preset_moead("moead.de"),
      decomp = decomp.loaded.2,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors,
      showpars = list(show.iters = "none", showevery = 10),
      seed = seed,
      update = update.restricted,
      resource.allocation = resource.allocation.RANDOM_FIXED.100,
    )

    moead.random.3.more.pressure <- moead(
      problem  = problem.DTLZ2,
      preset   = preset_moead("moead.de"),
      decomp = decomp2,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors,
      showpars = list(show.iters = "none", showevery = 10),
      seed = seed,
      update = update.best,
      resource.allocation = resource.allocation.RANDOM_FIXED.0.1
    )
    
    ref1 <-
      rbind(
        Yref,
        moead.de$Archive$Y,
        moead.de.diverge$Archive$Y,
        moead.random.3$Archive$Y,
        moead.random.50$Archive$Y,
        moead.random.100$Archive$Y,
        moead.random.3.more.pressure$Archive$Y
      )
    
plot.data.rand.3 <- data.frame.metrics(moead.random.3)
    plot.data.more.rand.3 <-
      data.frame.metrics(moead.random.3.more.pressure)
    plot.moead.de <- data.frame.metrics(moead.de)
    plot.moead.de.diverge <- data.frame.metrics(moead.de.diverge)
    plot.data.rand.50 <- data.frame.metrics(moead.random.50)
    plot.data.rand.100 <- data.frame.metrics(moead.random.100)
    
    plot.data3 <-
      rbind(
        data.frame(scaling_Y(Yref, ref1), Strategy = "Theoretical Pareto Front"),
        data.frame(scaling_Y(moead.random.3$Archive$Y[ecr::nondominated(t(moead.random.3$Archive$Y)),], ref1), Strategy = "PS - n = 3"),
        data.frame(scaling_Y(moead.random.50$Archive$Y[ecr::nondominated(t(moead.random.50$Archive$Y)),], ref1), Strategy = "PS - n = 50"),
        data.frame(scaling_Y(moead.random.100$Archive$Y[ecr::nondominated(t(moead.random.100$Archive$Y)),], ref1), Strategy = "PS - n = 100"),
        data.frame(
          scaling_Y(moead.random.3.more.pressure$Archive$Y[ecr::nondominated(t(moead.random.3.more.pressure$Archive$Y)),], ref1),
          Strategy = "Update all - PS - n = 3"
        ),
        data.frame(scaling_Y(moead.de$Archive$Y[ecr::nondominated(t(moead.de$Archive$Y)),], ref1), Strategy = "MOEA/D"),
        data.frame(scaling_Y(moead.de.diverge$Archive$Y[ecr::nondominated(t(moead.de.diverge$Archive$Y)),], ref1), Strategy = "MOEA/D diverge")
      )
    
    #### regression
    print(ggplot(plot.data.rand.3, aes(iteration, value)) + 
            ggtitle(paste0("MOEA/D-PS with n = 3 in ", fun)) + ylim(-1, 1) +
            geom_smooth(method = 'lm'))
    filename = paste0("~/pressure_study/",
                      fun ,
                      "regression_moeadps_3_pressure_study.png")
    ggsave(filename = filename,
           dpi = 600,
           width = 9)
    

    print(ggplot(plot.data.rand.50, aes(iteration, value)) +
            ggtitle(paste0("MOEA/D-PS with n = 50 in ", fun)) + ylim(-1, 1) +
            geom_smooth(method = 'lm'))
    filename = paste0("~/pressure_study/",
                      fun ,
                      "regression_moeadps_50_pressure_study.png")
    ggsave(filename = filename,
           dpi = 600,
           width = 9)
    
    print(ggplot(plot.data.rand.100, aes(iteration, value)) +
            ggtitle(paste0("MOEA/D-PS with n = 100 in ", fun)) + ylim(-1, 1) +
            geom_smooth(method = 'lm'))
    filename = paste0("~/pressure_study/",
                      fun ,
                      "regression_moeadps_100_pressure_study.png")
    ggsave(filename = filename,
           dpi = 600,
           width = 9)
    
    
    print(ggplot(plot.data.more.rand.3, aes(iteration, value)) +
            ggtitle(paste0("Update all in MOEA/D-PS with n = 3 in ", fun)) + ylim(-1, 1) +
            geom_smooth(method = 'lm'))
    filename = paste0("~/pressure_study/",
                      fun ,
                      "regression_moeadps_3_more_pressure_study.png")
    ggsave(filename = filename,
           dpi = 600,
           width = 9)
    
    print(ggplot(plot.moead.de, aes(iteration, value)) +
            ggtitle(paste0("MOEA/D-DE in ", fun)) + ylim(-1, 1) +
            geom_smooth(method = 'lm'))
    filename = paste0("~/pressure_study/",
                      fun ,
                      "regression_moead_pressure_study.png")
    ggsave(filename = filename,
           dpi = 600,
           width = 9)
    
    print(ggplot(plot.moead.de.diverge, aes(iteration, value)) +
            ggtitle(paste0("MOEA/D-DE diverge in ", fun)) + ylim(-1, 1) +
            geom_smooth(method = 'lm'))
    filename = paste0("~/pressure_study/",
                      fun ,
                      "regression_moead_diverge_pressure_study.png")
    ggsave(filename = filename,
           dpi = 600,
           width = 9)
    ####points
    
    print(ggplot(plot.data.rand.3, aes(iteration, value)) + geom_line(aes(color = metric)) + geom_point(aes(color = metric, shape = metric)) +
            ggtitle(paste0("MOEA/D-PS with n = 3 in ", fun))+ ylim(-1, 1))
    filename = paste0("~/pressure_study/",
                      fun ,
                      "points_moeadps_3_pressure_study.png")
    ggsave(filename = filename,
           dpi = 600,
           width = 9)
    
    print(ggplot(plot.data.rand.50, aes(iteration, value)) + geom_line(aes(color = metric)) + geom_point(aes(color = metric, shape = metric)) +
            ggtitle(paste0("MOEA/D-PS with n = 50 in ", fun))+ ylim(-1, 1))
    filename = paste0("~/pressure_study/",
                      fun ,
                      "points_moeadps_50_pressure_study.png")
    ggsave(filename = filename,
           dpi = 600,
           width = 9)
    
    print(ggplot(plot.data.rand.100, aes(iteration, value)) + geom_line(aes(color = metric)) + geom_point(aes(color = metric, shape = metric)) +
            ggtitle(paste0("MOEA/D-PS with n = 100 in ", fun))+ ylim(-1, 1))
    filename = paste0("~/pressure_study/",
                      fun ,
                      "points_moeadps_100_pressure_study.png")
    ggsave(filename = filename,
           dpi = 600,
           width = 9)
    
    print(ggplot(plot.data.more.rand.3, aes(iteration, value)) + geom_line(aes(color = metric)) + geom_point(aes(color = metric, shape = metric)) +
            ggtitle(paste0("Update all in MOEA/D-PS with n = 3 in ", fun))+ ylim(-1, 1))
    filename = paste0("~/pressure_study/",
                      fun ,
                      "points_moeadps_3_more_pressure_study.png")
    ggsave(filename = filename,
           dpi = 600,
           width = 9)
    
    print(ggplot(plot.moead.de, aes(iteration, value)) + geom_line(aes(color = metric)) + geom_point(aes(color = metric, shape = metric)) +
            ggtitle(paste0("MOEA/D-DE in", fun))+ ylim(-1, 1))
    filename = paste0("~/pressure_study/",
                      fun ,
                      "points_moead_pressure_study.png")
    ggsave(filename = filename,
           dpi = 600,
           width = 9)
    
    print(ggplot(plot.moead.de.diverge, aes(iteration, value)) + geom_line(aes(color = metric)) + geom_point(aes(color = metric, shape = metric)) +
            ggtitle(paste0("MOEA/D-DE diverge in", fun))+ ylim(-1, 1))
    filename = paste0("~/pressure_study/",
                      fun ,
                      "points_moead_diverge_pressure_study.png")
    ggsave(filename = filename,
           dpi = 600,
           width = 9)
    
    print(ggplot(plot.data3, aes(f1, f2)) + geom_point(aes(color = Strategy)) + ggtitle(paste0("PF of all methods - ", fun)))
    
    filename = paste0("~/pressure_study/", fun , "_pf_pressure_study.png")
    ggsave(filename = filename,
           dpi = 600,
           width = 9)
    
    
    
    
  }
}
