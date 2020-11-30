rm(list = ls(all = TRUE))
library(parallel)
library(nsga2R)
library(smoof)
library(MOEADps)
library(ggplot2)
library(eaf)
#which one?
library(emoa)
library(mco)
library(feather)

source("~/MOEADr/R/utils.R")

source('~/MOEADr/R/variation_de.R')
source('~/MOEADr/R/moead.R')

source('~/MaOEA/R/maoea.R')
source('~/MaOEA/R/NSGA3.r')
source('~/MaOEA/R/functions.R')
source('~/MaOEA/R/nsga2.RA.R')
source('~/MaOEA/R/R2mtch.R')

cores <-  16
cl <- makeCluster(cores)

# which bbob function
instance <- 1
# number of objectives
n.obj <- 2
pop.size <- 250
maxevals <- 100000

repetitions <- 10

# number of solutions/population size
nIndividual <- pop.size


decomp    <-
  list(name       = "sld", H = pop.size-1)


resource.allocation.50 <-
  list(
    name = "random",
    dt = 0,
    selection = "n",
    n = 48
  )

variation = preset_moead("moead.de")$variation

scaling <- list()
scaling$name <- "simple"
neighbors.500 <- preset_moead("moead.de")$neighbors
neighbors.500$T <- pop.size* 0.2
update <- preset_moead("moead.de")$update
update$UseArchive = TRUE

stopcrit  <- list(list(name    = "maxeval",
                       maxeval = maxevals))



resource.allocation.nsga3 <-
  list(
    name = "random",
    dt = 0,
    selection = "n",
    n = pop.size/10 - n.obj
  )

resource.allocation.nsga2 <-
  list(
    name = "random",
    dt = 0,
    selection = "n",
    n = pop.size/10 - n.obj
  )

ps_target <- 1 / (5 + ( 1 / 2  )^0.5 )
control <- list(successProbTarget=ps_target,crossoverProbability=0)

nsga3RA <- data.frame()
nsga3 <- data.frame()
nsga2 <- data.frame()
nsga2RA <- data.frame()
MOEADPS <- data.frame()
MOEAD <- data.frame()
multicma <- data.frame()
multicmaRA <- data.frame()

problem.to.solve <- c("BIBBOB55", "BIBBOB54", "BIBBOB53", "BIBBOB50", "BIBBOB47", "BIBBOB46", "BIBBOB41", "BIBBOB36", "BIBBOB35", "BIBBOB28", "BIBBOB21", "BIBBOB20", "BIBBOB11", "BIBBOB2", "BIBBOB1")

for (fun in problem.to.solve) {
  benchmark <- strsplit(fun, "[0-9]")[[1]][1]

  if(benchmark == "UF"){
    # dimensions
    dimensions <- 100
    number <- as.integer(strsplit(fun, "[A-Z]")[[1]][3])
    problem <-
      makeUFFunction(dimensions = dimensions, id = number)
  }
  else{
    # dimensions
    dimensions <- 40

    number <- as.integer(strsplit(fun, "[A-Z]")[[1]][7])
    problem <-
      makeBiObjBBOBFunction(dimensions = dimensions, fid = number, iid = instance)
  }
  problem.BiBBOB <- function(X) {
    t(apply(X, MARGIN = 1,
            FUN = problem))
  }

  par.set = ParamHelpers::getParamSet(problem)
  problem.bibbob <- list(
    name       = "problem.BiBBOB",
    xmin       = as.numeric(getLower(par.set)),
    xmax       = as.numeric(getUpper(par.set)),
    m          = n.obj
  )
  # variation stack
  ctrl <- list(
    weightVector = t(loaded.weights.500),
    crossoverProbability = 20,
    mutationProbability = 1 / dimensions,
    lower       = as.numeric(getLower(par.set)),
    upper       = as.numeric(getUpper(par.set))
  )
  variation[[2]]$pm = 1 / dimensions

  print(benchmark)
  print(number)

  for (i in 1:repetitions) {
    # random population definitions
    X  <- create_population(N       = pop.size,
                            problem = problem.bibbob)

    dir.name <- paste0("~/tec/nsga_3/",i,"/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)
    nsga.3 <-
      optimMaOEA(
        t(X),
        problem.bibbob,
        NSGA3,
        n.obj,
        maxevals,
        dimensions,
        nIndividual,
        ctrl,
        saving.dir = dir.name
      )
    nsga.3$y <- t(nsga.3$y)

    dir.name <- paste0("~/tec/nsga_3_ra/",i,"/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)
    nsga.3.RA <-
      optimMaOEA(
        t(X),
        problem.bibbob,
        NSGA3,
        n.obj,
        maxevals,
        dimensions,
        nIndividual,
        ctrl,
        resource.allocation.nsga3,
        saving.dir = dir.name
      )
    nsga.3.RA$y <- t(nsga.3.RA$y)

    dir.name <- paste0("~/tec/moead_25/",i,"/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)
    moead.ps.50 <- moeadps(
      problem  = problem.bibbob,
      preset   = preset_moead("moead.de"),
      decomp = decomp,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors.500,
      showpars = list(show.iters = "none", showevery = 1000),
      update = update,
      resource.allocation = resource.allocation.50,
      saving.dir = dir.name,
      X
    )

    dir.name <- paste0("~/tec/moead/",i,"/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)
    moead <- moeadps(
      problem  = problem.bibbob,
      preset   = preset_moead("moead.de"),
      decomp = decomp,
      variation = variation,
      stopcrit = stopcrit,
      scaling = scaling,
      neighbors = neighbors.500,
      showpars = list(show.iters = "none", showevery = 1000),
      update = update,
      X
    )

    dir.name <- paste0("~/tec/nsga_2/",i,"/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)
    nsga.2 <-
      nsgaps(
        fn = problem,
        varNo = dimensions,
        objDim = n.obj,
        lowerBounds = rep(as.numeric(getLower(par.set)), dimensions),
        upperBounds = rep(as.numeric(getUpper(par.set)), dimensions),
        popSize = pop.size,
        tourSize = 2,
        maxiter = maxevals,
        cprob = 0.9,
        XoverDistIdx = 20,
        mprob = 0.1,
        MuDistIdx = 3,
        parent = X,
        saving.dir = dir.name
      )

    dir.name <- paste0("~/tec/nsga_2_ra/",i,"/")
    if (!dir.exists(dir.name))
      dir.create(dir.name)

    nsga.2.ra <-
      nsgaps(
        fn = problem,
        varNo = dimensions,
        objDim = n.obj,
        lowerBounds = rep(as.numeric(getLower(par.set)), dimensions),
        upperBounds = rep(as.numeric(getUpper(par.set)), dimensions),
        popSize = pop.size,
        tourSize = 2,
        maxiter = maxevals,
        cprob = 0.9,
        XoverDistIdx = 20,
        mprob = 0.1,
        MuDistIdx = 3,
        resource.allocation = resource.allocation.nsga2,
        parent = X,
        saving.dir = dir.name
      )


    colnames(nsga.2$objectives) <- c("V1", "V2")
    colnames(nsga.2.ra$objectives) <- c("V1", "V2")


    ref1 <-
      as.data.frame(
        rbind(
          nsga.3$y,
          nsga.3.RA$y,
          nsga.2$objectives,
          nsga.3.RA$objectives,
          moead.ps.50$Y
        )
      )

    nsga3RA <- rbind(nsga3RA, cbind(algo = nsga.3.RA$y, iter = i))
    nsga3 <- rbind(nsga3, cbind(algo = nsga.3$y, iter = i))

    nsga2RA <-
      rbind(nsga2RA, cbind(algo = nsga.2.ra$objectives, iter = i))
    nsga2 <- rbind(nsga2, cbind(algo = nsga.2$objectives, iter = i))

    MOEADPS <- rbind(MOEADPS, cbind(algo = moead.ps.50$Y, iter = i))
    MOEAD <- rbind(MOEAD, cbind(algo = moead$Y, iter = i))

  }
  NSGA3.  <- data.frame()
  NSGA3.RA <- data.frame()
  NSGA2  <- data.frame()
  NSGA2.RA <- data.frame()
  MOEA.D.PS <- data.frame()
  MOEA.D <- data.frame()

  for (j in 1:1) {
    #nsga-3
    nsga.3$scalingY <- nsga3[nsga3$iter == j, ]
    ranks.1 <- pareto_rank(nsga.3$scalingY)
    temp1 <- as.data.frame(nsga.3$scalingY[ranks.1 == 1,])
    colnames(temp1) <- colnames(ref1)
    nsga.3$scalingY <- scaling_Y(temp1[, 1:2], ref1)
    NSGA3.  <-
      rbind(NSGA3., data.frame(HV = hypervolume(nsga.3$scalingY, c(1, 1), maximise = F)))
    # nsga-3 with RA
    nsga.3.RA$scalingY <- nsga3RA[nsga3RA$iter == j, ]
    ranks.2 <- pareto_rank(nsga.3.RA$scalingY)
    temp2 <- as.data.frame(nsga.3.RA$scalingY[ranks.2 == 1,])
    colnames(temp2) <- colnames(ref1)
    nsga.3.RA$scalingY <- scaling_Y(temp2[, 1:2], ref1)
    NSGA3.RA <-
      rbind(NSGA3.RA, data.frame(HV = hypervolume(
        nsga.3.RA$scalingY, c(1, 1), maximise = F
      )))
    #nsga-2
    nsga.2$scalingY <- nsga2[nsga2$iter == j, ]
    ranks.3 <- pareto_rank(nsga.2$scalingY)
    temp3 <- as.data.frame(nsga.2$scalingY[ranks.3 == 1,])
    colnames(temp3) <- colnames(ref1)
    nsga.2$scalingY <- scaling_Y(temp3[, 1:2], ref1)
    NSGA2  <-
      rbind(NSGA2, data.frame(HV = hypervolume(nsga.2$scalingY, c(1, 1), maximise = F)))
    # nsga-2 with RA
    nsga.2.ra$scalingY <- nsga2RA[nsga2RA$iter == j, ]
    ranks.4 <- pareto_rank(nsga.2.ra$scalingY)
    temp4 <- as.data.frame(nsga.2.ra$scalingY[ranks.4 == 1,])
    colnames(temp4) <- colnames(ref1)
    nsga.2.ra$scalingY <- scaling_Y(temp4[, 1:2], ref1)
    NSGA2.RA <-
      rbind(NSGA2.RA, data.frame(HV = hypervolume(
        nsga.2.ra$scalingY, c(1, 1), maximise = F
      )))
    # MOEA/D-PS
    moead.ps.50$scalingY <- MOEADPS[MOEADPS$iter == j, ]
    ranks.5 <- eaf::pareto_rank(moead.ps.50$scalingY)
    temp5 <- as.data.frame(moead.ps.50$scalingY[ranks.5 == 1,])
    colnames(temp5) <- colnames(ref1)
    moead.ps.50$scalingY <- scaling_Y(temp5[, 1:2], ref1)
    MOEA.D.PS <-
      rbind(MOEA.D.PS, data.frame(HV = hypervolume(
        moead.ps.50$scalingY, c(1, 1), maximise = F
      )))
    # MOEA/D
    moead$scalingY <- MOEAD[MOEAD$iter == j, ]
    ranks.6 <- eaf::pareto_rank(moead$scalingY)
    temp6 <- as.data.frame(moead$scalingY[ranks.6 == 1,])
    colnames(temp6) <- colnames(ref1)
    moead$scalingY <- scaling_Y(temp6[, 1:2], ref1)
    MOEA.D <-
      rbind(MOEA.D, data.frame(HV = hypervolume(moead$scalingY, c(1, 1), maximise = F)))
  }


  data <- data.frame(rbind(
    cbind(HV = NSGA3., name = "NSGA-III"),
    cbind(HV = NSGA3.RA, name = "NSGA-III-PS"),
    cbind(HV = NSGA2, name = "NSGA-II"),
    cbind(HV = NSGA2.RA, name = "NSGA-II-PS"),
    cbind(HV = MOEA.D, name = "MOEA/D"),
    cbind(HV = MOEA.D.PS, name = "MOEA/D-PS")

  ))

  data$HV <- as.numeric(data$HV)
  plot <-
    ggplot(data, aes(x = name, y = HV, fill = name)) + geom_boxplot() +
    theme_minimal(base_size = 26) +
    labs(x = "Strategy", y = "HV", face = "bold") +
    scale_fill_discrete(guide = FALSE) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

  print(plot + theme(legend.position = "none", axis.text = element_text(size =
                                                                          15)))


  filename = paste0("~/Desktop/boxplot_RAs_temp", fun,".png")
  ggsave(
    filename = filename,
    dpi = 300,
    width = 14,
    height = 14
  )
  median <-
    aggregate(data$HV, median, by = list(data$name))
  colnames(median) <- c("Name", "HV")
  sd <-
    aggregate(data$HV, sd, by = list(data$name))
  print(data.frame(median, SD = sd[,2]))
}

stopCluster(cl)
