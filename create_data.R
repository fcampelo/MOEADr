## start loadind data
setwd("~/MOEADr/")
source("graphical_analysis.R")

nPop <- 350
nRun <- 30
nObj <- 2 # fix even if single obj
# number of variables
nVar <- 200
# number of constraints
nCon <- 1
# number of generations
gen <- 200
# reference point

refPoint <- c(1, 1)

# things
objsColnames <- paste("#obj", 1:nObj, sep = "")
varsColnames <- paste("#var", 1:nVar, sep = "")
consColnames <- paste("#con", 1:nCon, sep = "")

# init with zero
initialValueM <- 0
initialValue <<- initialValueM
indicatorArc <<- matrix(initialValue, nrow = 1)
indicatorTmp <<- matrix(initialValue, nrow = nRun)
indicatorArcIGD <<- matrix(initialValue, nrow = nRun)

# Fun names
fun.names <- list()
for (i in 1:7) {
  fun.names[[length(fun.names) + 1]] = paste0("UF", i)
  # fun.names[[length(fun.names) + 1]] = paste0("DTLZ", i)
}
# variants names
variants <- c("de", "norm", "rad", "random", "gra", "dra")

for (fun in fun.names) {
  print(fun)
  # Yref <- as.matrix(read.table(paste0("inst/extdata/pf_data/DTLZ",id, ".2D.pf")))
  Yref <-
    as.matrix(read.table(paste0("inst/extdata/pf_data/", fun, ".dat")))
  for (variant in variants) {
    print(variant)
    runIdPre <- paste0("../", variant)
    for (iRun2 in 1:nRun) {
      iRun <- iRun2 - 1
      my.data <- matrix(, nrow = 0, ncol = nObj + nVar + nCon + 2)
      my.data <-
        read.data(
          fun = fun,
          runIdPre = runIdPre,
          iRun = iRun,
          my.data = my.data,
          gen = gen
        )
      my.data[, 1:2] <-
        (sweep(my.data[, 1:2], 2, min.val)) / ((max.val - min.val) + 1e-50)
      isFeasible <- extractFeasible(my.data)
      cat("\n", sum(isFeasible), " solutions are feasible\n", sep = "")
      
      isFoundFeasible <<- T
      objsTmp <- extractNonDominatedSolutions(my.data, isFeasible)
      objsTmp2 <- convertEvalObjectives(objsTmp)
      rankFeasible <- rankUnion(objsTmp2, T) #T or F???
      archive <<- subset(archive, rankFeasible == 1)
      cat(sum(rankFeasible == 1),
          " solutions are nondominated\n",
          sep = "")
      
      calculateIndicator(iRun2, objsTmp2, ref.points)
      indicatorArcIGD[iRun2] <- calcIGD(archive[, 1:2], Yref)
      
    }
    cat("HV", indicatorArc, "\n")
    cat("IGD", indicatorArcIGD, "\n")
    
    if (variant == "de")
      de_median_gen <-
      cbind(indicatorArc,
            indicatorArcIGD,
            1:length(indicatorArcIGD),
            "de")
    else if (variant == "rad")
      rad_median_gen <-
      cbind(indicatorArc,
            indicatorArcIGD,
            1:length(indicatorArcIGD),
            "rad")
    else if (variant == "dra")
      dra_median_gen <-
      cbind(indicatorArc,
            indicatorArcIGD,
            1:length(indicatorArcIGD),
            "dra")
    else if (variant == "gra")
      gra_median_gen <-
      cbind(indicatorArc,
            indicatorArcIGD,
            1:length(indicatorArcIGD),
            "gra")
    else if (variant == "norm")
      norm_median_gen <-
      cbind(indicatorArc,
            indicatorArcIGD,
            1:length(indicatorArcIGD),
            "norm")
    else if (variant == "random")
      random_median_gen <-
      cbind(indicatorArc,
            indicatorArcIGD,
            1:length(indicatorArcIGD),
            "random")
    
  }
}

colnames(de_median_gen) <- c("HV", "IGD", "gen", "name")
colnames(rad_median_gen) <- c("HV", "IGD", "gen", "name")
colnames(dra_median_gen) <- c("HV", "IGD", "gen", "name")
colnames(gra_median_gen) <- c("HV", "IGD", "gen", "name")
colnames(norm_median_gen) <- c("HV", "IGD", "gen", "name")
colnames(random_median_gen) <- c("HV", "IGD", "gen", "name")


df <-
  rbind(
    de_median_gen,
    rad_median_gen,
    gra_median_gen,
    random_median_gen,
    random_median_gen,
    norm_median_gen
  )
df <- as.data.frame(df)
# names(df) <- c("HV")

pathname <- "../files/hv_all.png"
p <- ggplot(df, aes(gen, HV, group = name)) +
  geom_line(aes(color = name)) +
  geom_point(aes(shape = name, color = name))#+
# geom_errorbar(aes(ymin=HV-sd_HV, ymax=HV+sd_HV, color = name), width=.1)
# p
ggsave(filename = pathname, device = "png")

pathname <- "../files/igd_all.png"
p <- ggplot(df, aes(gen, IGD, group = name)) +
  geom_line(aes(color = name)) +
  geom_point(aes(shape = name, color = name))#+
# geom_errorbar(aes(ymin=IGD-sd_IGD, ymax=IGD+sd_IGD, color=name, alpha=0.8), width=.1)
# p
ggsave(filename = pathname, device = "png")



my.data10 <- (matrix(ncol = 2, nrow = 0))
colnames(my.data10) <- c("HV", "IGD")
my.data10 <-
  rbind(
    my.data10,
    de_median_gen[nrow(de_median_gen),],
    rad_median_gen[nrow(rad_median_gen),],
    dra_median_gen[nrow(dra_median_gen),],
    gra_median_gen[nrow(gra_median_gen),],
    norm_median_gen[nrow(norm_median_gen),],
    random_median_gen[nrow(random_median_gen),]
  )
my.data10 <- as.data.frame(my.data10)
my.data10$algorithm <- variants
create_graphs(my.data10, fun.names, 2)
