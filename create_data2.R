## start loadind data
rm(list = ls(all = TRUE))
setwd("~/MOEADr/")
source("graphical_analysis.R")
setwd("~/MOEADr/")
nPop <- 350
nRun <- 30
nObj <- 2 # fix even if single obj
# number of variables
nVar <- 100
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
for (i in 1:1) {
  fun.names[[length(fun.names) + 1]] = paste0("UF", i)
  # fun.names[[length(fun.names) + 1]] = paste0("DTLZ", i)
}
# variants names
# variants <- c("norm", "rad")
# variants <- c("de", "norm", "rad", "random", "gra", "dra")
variants <- c("de", "norm", "rad", "random", "gra")

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
for (i in 1:1) {
  fun.names[[length(fun.names) + 1]] = paste0("UF", i)
  # fun.names[[length(fun.names) + 1]] = paste0("DTLZ", i)
}

max.val <- c(-Inf, -Inf)
min.val <- c(Inf, Inf)
# id <- 1
for (variant in variants) {
  runIdPre <- paste0("../", variant)
  print(variant)
  for (fun in fun.names) {
    print(fun)
    for (iRun2 in 1:nRun) {
      iRun <- iRun2 - 1
      my.data <-
        read.data(
          fun = fun,
          runIdPre = runIdPre,
          iRun = iRun,
          gen = gen,
          flag = 0
        )$my.data2[,1:2]
      max.val <- pmax(max.val, apply(my.data, 2, max))
      min.val <- pmin(min.val, apply(my.data, 2, min))
    }
  }
}

de_median_gen <- data.frame()
# dra_median_gen <- data.frame()
gra_median_gen <- data.frame()
random_median_gen <- data.frame()
rad_median_gen <- data.frame()
norm_median_gen <- data.frame()

de_median_hvs <- data.frame()
# dra_median_hvs <- data.frame()
gra_median_hvs <- data.frame()
random_median_hvs <- data.frame()
rad_median_hvs <- data.frame()
norm_median_hvs <- data.frame()

de_median_igds <- data.frame()
# dra_median_igds <- data.frame()
gra_median_igds <- data.frame()
random_median_igds <- data.frame()
rad_median_igds <- data.frame()
norm_median_igds <- data.frame()
for (fun in fun.names) {
  print(fun)
  # Yref <- as.matrix(read.table(paste0("inst/extdata/pf_data/DTLZ",id, ".2D.pf")))
  Yref <-
    as.matrix(read.table(paste0("inst/extdata/pf_data/", fun, ".dat")))
  for (variant in variants) {
    print(variant)
    runIdPre <- paste0("../", variant)
    df <- data.frame()
    df2 <- data.frame()
    for (iRun2 in 1:nRun) {
      iRun <- iRun2 - 1
      # my.data <- matrix(, nrow = 0, ncol = nObj + nVar + nCon + 2)
      out <-
        read.data(
          fun = fun,
          runIdPre = runIdPre,
          iRun = iRun,
          gen = gen,
          flag = 1
        )
      my.data <- out$my.data2
      
      if (variant == "de"){
        de_median_hvs <- rbind(de_median_hvs, data.frame(out$hv, rep(1:gen)))
        de_median_igds <- rbind(de_median_igds, data.frame(out$igd, rep(1:gen)))
      } 
      else if (variant == "rad"){
        rad_median_hvs <- rbind(rad_median_hvs, data.frame(out$hv, rep(1:gen)))
        rad_median_igds <- rbind(rad_median_igds, data.frame(out$igd, rep(1:gen)))
      } 
      # else if (variant == "dra"){
      #   dra_median_hvs <- rbind(dra_median_hvs, data.frame(out$hv, rep(1:gen)))
      #   dra_median_igds <- rbind(dra_median_igds, data.frame(out$igd, rep(1:gen)))
      # } 
      else if (variant == "gra"){
        gra_median_hvs <- rbind(gra_median_hvs, data.frame(out$hv, rep(1:gen)))
        gra_median_igds <- rbind(gra_median_igds, data.frame(out$igd, rep(1:gen)))
      } 
      else if (variant == "norm"){
        norm_median_hvs <- rbind(norm_median_hvs, data.frame(out$hv, rep(1:gen)))
        norm_median_igds <- rbind(norm_median_igds, data.frame(out$igd, rep(1:gen)))
      }
      else if (variant == "random"){
        random_median_hvs <- rbind(random_median_hvs, data.frame(out$hv, rep(1:gen)))
        random_median_igds <- rbind(random_median_igds, data.frame(out$igd, rep(1:gen)))
      } 
      
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
      
      calculateIndicator(iRun2, objsTmp2, refPoint)
      if (is.null(archive)) archive <- objsTmp2
      indicatorArcIGD[iRun2] <- calcIGD(archive[, 1:2], Yref)
      
      
    }
    
    
    if (variant == "de") {
      de_median_gen <-
        cbind(indicatorTmp,
              indicatorArcIGD,
              1:length(indicatorArcIGD),
              "de")
      colnames(de_median_gen) <- c("HV", "IGD", "gen", "name")
    }
    else if (variant == "rad") {
      rad_median_gen <-
        cbind(indicatorTmp,
              indicatorArcIGD,
              1:length(indicatorArcIGD),
              "rad")
      colnames(rad_median_gen) <- c("HV", "IGD", "gen", "name")
    }
    # else if (variant == "dra") {
    #   dra_median_gen <-
    #     cbind(indicatorTmp,
    #           indicatorArcIGD,
    #           1:length(indicatorArcIGD),
    #           "dra")
    #   colnames(dra_median_gen) <- c("HV", "IGD", "gen", "name")
    # }
    else if (variant == "gra") {
      gra_median_gen <-
        cbind(indicatorTmp,
              indicatorArcIGD,
              1:length(indicatorArcIGD),
              "gra")
      colnames(gra_median_gen) <- c("HV", "IGD", "gen", "name")
    }
    else if (variant == "norm") {
      norm_median_gen <-
        cbind(indicatorTmp,
              indicatorArcIGD,
              1:length(indicatorArcIGD),
              "norm")
      colnames(norm_median_gen) <- c("HV", "IGD", "gen", "name")
    }
    else if (variant == "random") {
      random_median_gen <-
        cbind(indicatorTmp,
              indicatorArcIGD,
              1:length(indicatorArcIGD),
              "random")
      colnames(random_median_gen) <- c("HV", "IGD", "gen", "name")
    }
  }
  df <-
    rbind(
      df,
      data.frame(de_median_gen),
      data.frame(rad_median_gen),
      data.frame(gra_median_gen),
      # data.frame(dra_median_gen),
      data.frame(random_median_gen),
      data.frame(norm_median_gen)
    )
  df$fun <- fun
}


#
#
#
# #
# #

names(de_median_hvs) <- c("HV", "gen")
names(rad_median_hvs) <- c("HV", "gen")
# names(dra_median_hvs) <- c("HV", "gen")
names(gra_median_hvs) <- c("HV", "gen")
names(random_median_hvs) <- c("HV", "gen")
names(norm_median_hvs) <- c("HV", "gen")
de_hv.plot <-aggregate(x = de_median_hvs$HV, FUN = median, by = list(de_median_hvs$gen))
de_hv.plot$name <- "de"
rad_hv.plot <-aggregate(x = rad_median_hvs$HV, FUN = median, by = list(rad_median_hvs$gen))
rad_hv.plot$name <- "rad"
# dra_hv.plot <-aggregate(x = dra_median_hvs$HV, FUN = median, by = list(dra_median_hvs$gen))
# dra_hv.plot$name <- "dra"
gra_hv.plot <-aggregate(x = gra_median_hvs$HV, FUN = median, by = list(gra_median_hvs$gen))
gra_hv.plot$name <- "gra"
random_hv.plot <-aggregate(x = random_median_hvs$HV, FUN = median, by = list(random_median_hvs$gen))
random_hv.plot$name <- "random"
norm_hv.plot <-aggregate(x = norm_median_hvs$HV, FUN = median, by = list(norm_median_hvs$gen))
norm_hv.plot$name <- "norm"
df2 <-
  rbind(
    data.frame(de_hv.plot),
    data.frame(rad_hv.plot),
    # data.frame(dra_hv.plot),
    data.frame(gra_hv.plot),
    data.frame(random_hv.plot),
    data.frame(norm_hv.plot)
  )

names(de_median_igds) <- c("igd", "gen")
names(rad_median_igds) <- c("igd", "gen")
# names(dra_median_igds) <- c("igd", "gen")
names(gra_median_igds) <- c("igd", "gen")
names(random_median_igds) <- c("igd", "gen")
names(norm_median_igds) <- c("igd", "gen")
de_igd.plot <-aggregate(x = de_median_igds$igd, FUN = median, by = list(de_median_igds$gen))
de_igd.plot$name <- "de"
rad_igd.plot <-aggregate(x = rad_median_igds$igd, FUN = median, by = list(rad_median_igds$gen))
rad_igd.plot$name <- "rad"
# dra_igd.plot <-aggregate(x = dra_median_igds$igd, FUN = median, by = list(dra_median_igds$gen))
# dra_igd.plot$name <- "dra"
gra_igd.plot <-aggregate(x = gra_median_igds$igd, FUN = median, by = list(gra_median_igds$gen))
gra_igd.plot$name <- "gra"
random_igd.plot <-aggregate(x = random_median_igds$igd, FUN = median, by = list(random_median_igds$gen))
random_igd.plot$name <- "random"
norm_igd.plot <-aggregate(x = norm_median_igds$igd, FUN = median, by = list(norm_median_igds$gen))
norm_igd.plot$name <- "norm"
df3 <-
  rbind(
    data.frame(de_igd.plot),
    data.frame(rad_igd.plot),
    # data.frame(dra_igd.plot),
    data.frame(gra_igd.plot),
    data.frame(random_igd.plot),
    data.frame(norm_igd.plot)
  )
names(df3) <- c("gen", "IGD", "name")
names(df2) <- c("gen", "HV", "name")
pathname <- "../files/hv_all.png"
p2 <- ggplot(df2, aes(gen, HV, group=name)) +
  geom_line(aes(color=name))+
  geom_point(aes(shape=name, color = name))#+
p2
ggsave(filename = pathname, device = "png")
# #
pathname <- "../files/igd_all.png"
p3 <- ggplot(df3, aes(gen, IGD, group=name)) +
  geom_line(aes(color=name))+
  geom_point(aes(shape=name, color = name))#+
# p3
ggsave(filename = pathname, device = "png")
#
#
#
df$HV <- round(as.numeric(levels(df$HV))[df$HV], 4)
df$IGD <- round(as.numeric(levels(df$IGD))[df$IGD], 4)
df$algorithm <- df$name

create_graphs(df, fun.names, 2)
# p <- ggplot(df, aes(algorithm, HV, color = name)) +
#   geom_boxplot(aes(fill = algorithm), scale = "count")
# p
# ""


write_feather(df, "forboxplot")
write_feather(df2, "HV_gens")
write_feather(df3, "IGD_gens")