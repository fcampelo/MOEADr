## start loadind data
rm(list = ls(all = TRUE))
setwd("~/MOEADr/")
source("graphical_analysis.R")
setwd("~/MOEADr/")
nPop <- 350
# nRun <- 21
nRun <- 21
nObj <- 2 # fix even if single obj
# number of variables
nVar <- 100
# number of constraints
nCon <- 1
# reference point

refPoint <- c(1, 1)

#evaluation points
# check.points <- c(20000, 40000, 60000, 80000, 100000, 120000, 140000)

# things
objsColnames <- paste("#obj", 1:nObj, sep = "")
varsColnames <- paste("#var", 1:nVar, sep = "")
consColnames <- paste("#con", 1:nCon, sep = "")


# Fun names
fun.names <- list()
for (i in 1:3) {
  # fun.names[[length(fun.names) + 1]] = paste0("UF", i)
  fun.names[[length(fun.names) + 1]] = paste0("DTLZ", i)
}
variants <- c("de", "norm", "rad", "random", "gra")
# variants <- c("de", "norm", "random", "gra")
# variants <- c("random ")

n.obj <- 2
for (fun in fun.names) {
  print(fun)
  # things
  objsColnames <- paste("#obj", 1:nObj, sep = "")
  varsColnames <- paste("#var", 1:nVar, sep = "")
  consColnames <- paste("#con", 1:nCon, sep = "")
  
  
  
  max.val <- c(-Inf, -Inf)
  min.val <- c(Inf, Inf)
  id <- 1
  #loop for getting max/min values
  print("first variant for")
  for (variant in variants) {
    runIdPre <- paste0("../", variant)
    # temp <- read_feather(paste0("../",variant, "/UF1_info"))
    temp <- read_feather(paste0("../", variant, "/", fun,"_",n.obj, "_info"))
    temp <- as.data.frame(temp)
    for (iRun2 in 1:nRun) {
      iRun <- iRun2 - 1
      
      gen <- as.integer(temp[iRun2, ])
      my.data <-
        read.data(
          fun = fun,
          runIdPre = runIdPre,
          iRun = iRun,
          my.gen = gen,
          flag = 0,
          n.obj=n.obj
        )$my.data2[, 1:2]
      max.val <- pmax(max.val, apply(my.data, 2, max))
      min.val <- pmin(min.val, apply(my.data, 2, min))
    }
  }
  
  de_median_gen <- data.frame()
  gra_median_gen <- data.frame()
  random_median_gen <- data.frame()
  rad_median_gen <- data.frame()
  norm_median_gen <- data.frame()
  
  de_median_hvs <- data.frame()
  gra_median_hvs <- data.frame()
  random_median_hvs <- data.frame()
  rad_median_hvs <- data.frame()
  norm_median_hvs <- data.frame()
  
  de_median_igds <- data.frame()
  gra_median_igds <- data.frame()
  random_median_igds <- data.frame()
  rad_median_igds <- data.frame()
  norm_median_igds <- data.frame()
  
  Yref <-
    as.matrix(read.table(paste0("inst/extdata/pf_data/", fun, ".dat")))
  #loop for getting scaled data, HV/IGD values over evaluations for all iteractions
  print("2nd variant for")
  for (variant in variants) {
    # init with zero
    print(variant)
    initialValueM <- 0
    initialValue <<- initialValueM
    indicatorArc <<- matrix(initialValue, nrow = 1)
    indicatorTmp <<- matrix(initialValue, nrow = nRun)
    indicatorArcIGD <<- matrix(initialValue, nrow = nRun)
    
    times <- list()
    ndom <- list()
    fes <- list()
    runIdPre <- paste0("../", variant)
    df <- data.frame()
    df2 <- data.frame()
    # temp <- read_feather(paste0("../",variant, "/UF1_info"))
    temp <- read_feather(paste0("../", variant, "/", fun,"_",n.obj, "_info"))
    temp <- as.data.frame(temp)
    for (iRun2 in 1:nRun) {
      iRun <- iRun2 - 1
      
      
      gen <- as.integer(temp[iRun2, ])
      
      out <-
        read.data(
          fun = fun,
          runIdPre = runIdPre,
          iRun = iRun,
          my.gen = gen,
          flag = 2,
          n.obj=n.obj
        )
      my.data <- out$my.data2
      
      if (variant == "de") {
        de_median_hvs <-
          rbind(de_median_hvs, data.frame(out$hv, rep(1:gen), out$nfe))
        de_median_igds <-
          rbind(de_median_igds, data.frame(out$igd, rep(1:gen), out$nfe))
      }
      else if (variant == "rad") {
        rad_median_hvs <-
          rbind(rad_median_hvs, data.frame(out$hv, rep(1:gen), out$nfe))
        rad_median_igds <-
          rbind(rad_median_igds, data.frame(out$igd, rep(1:gen), out$nfe))
      }
      else if (variant == "gra") {
        gra_median_hvs <-
          rbind(gra_median_hvs, data.frame(out$hv, rep(1:gen), out$nfe))
        gra_median_igds <-
          rbind(gra_median_igds, data.frame(out$igd, rep(1:gen), out$nfe))
      }
      else if (variant == "norm") {
        norm_median_hvs <-
          rbind(norm_median_hvs, data.frame(out$hv, rep(1:gen), out$nfe))
        norm_median_igds <-
          rbind(norm_median_igds, data.frame(out$igd, rep(1:gen), out$nfe))
      }
      else if (variant == "random") {
        random_median_hvs <-
          rbind(random_median_hvs, data.frame(out$hv, rep(1:gen), out$nfe))
        random_median_igds <-
          rbind(random_median_igds,
                data.frame(out$igd, rep(1:gen), out$nfe))
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
      if (is.null(archive))
        archive <- objsTmp2
      # indicatorArcIGD[iRun2] <- calcIGD(archive[, 1:2], Yref)
      pdGen <- formatC(gen,
                       width = 3,
                       format = "d",
                       flag = "0")
      times[[length(times) + 1]] <-
        as.integer(read_feather(paste0(
          runIdPre, "/", fun, "_time_", iRun + 1, "_", pdGen
        )))
      fes[[length(fes) + 1]] <- sum(isFeasible)
      ndom[[length(ndom) + 1]] <- sum(rankFeasible == 1)
    }
    if (variant == "de") {
      de_median_gen <-
        cbind(
          indicatorTmp,
          indicatorArcIGD,
          fes,
          ndom,
          times,
          1:length(indicatorArcIGD),
          "None"
        )
      colnames(de_median_gen) <-
        c("HV",
          "IGD",
          "fesiable",
          "nondominated",
          "time",
          "rep",
          "name")
    }
    else if (variant == "rad") {
      rad_median_gen <-
        cbind(
          indicatorTmp,
          indicatorArcIGD,
          fes,
          ndom,
          times,
          1:length(indicatorArcIGD),
          "MRDL"
        )
      colnames(rad_median_gen) <-
        c("HV",
          "IGD",
          "fesiable",
          "nondominated",
          "time",
          "rep",
          "name")
    }
    else if (variant == "gra") {
      gra_median_gen <-
        cbind(
          indicatorTmp,
          indicatorArcIGD,
          fes,
          ndom,
          times,
          1:length(indicatorArcIGD),
          "R.I."
        )
      colnames(gra_median_gen) <-
        c("HV",
          "IGD",
          "fesiable",
          "nondominated",
          "time",
          "rep",
          "name")
    }
    else if (variant == "norm") {
      norm_median_gen <-
        cbind(
          indicatorTmp,
          indicatorArcIGD,
          fes,
          ndom,
          times,
          1:length(indicatorArcIGD),
          "Norm"
        )
      colnames(norm_median_gen) <-
        c("HV",
          "IGD",
          "fesiable",
          "nondominated",
          "time",
          "rep",
          "name")
    }
    else if (variant == "random") {
      random_median_gen <-
        cbind(
          indicatorTmp,
          indicatorArcIGD,
          fes,
          ndom,
          times,
          1:length(indicatorArcIGD),
          "Random"
        )
      colnames(random_median_gen) <-
        c("HV",
          "IGD",
          "fesiable",
          "nondominated",
          "time",
          "rep",
          "name")
    }
  }
  df <-
    rbind(
      df,
      data.frame(de_median_gen),
      data.frame(rad_median_gen),
      data.frame(gra_median_gen),
      data.frame(random_median_gen),
      data.frame(norm_median_gen)
    )
  df$fun <- fun
  df$HV <- as.numeric(unlist(df$HV))
  df$IGD <- as.numeric(unlist(df$IGD))
  df$HV <- round(df$HV, 4)
  df$IGD <- round(df$IGD, 4)
  df$algorithm <- unlist(df$name)
  
  df$fesiable <- as.numeric(df$fesiable)
  df$nondominated <- as.numeric(df$nondominated)
  df$time <- as.numeric(df$time)
  df$rep <- as.numeric(df$rep)
  df$name <- as.character(df$name)
  
  #HV/IGD values over evaluations for "median" iteraction
  print("last variant for")
  variants<- c("None", "MRDL", "Random", "R.I.", "Norm")
  # variants<- c("None")
  for (variant in variants) {
    none.median <-
      df[which(df[df$algorithm == variant, ]$HV == median(df[df$algorithm == variant, ]$HV)), ]
  
    cat(variant, ": ",none.median$rep)
    
    if (variant == "None") name = "de"
    else if (variant == "MRDL") name = "rad"
    else if (variant == "R.I.") name = "gra"
    else if (variant == "Random") name = "random"
    else if (variant == "Norm") name = "norm"
    
    runIdPre <- paste0("../", name)
    temp <- read_feather(paste0("../", name, "/", fun, "_info"))
    temp <- as.data.frame(temp)
    
    iRun2 <- none.median$rep[1]
    iRun <- iRun2 - 1
    gen <- as.integer(temp[iRun2, ])
    
    out <-
      read.data(
        fun = fun,
        runIdPre = runIdPre,
        iRun = iRun,
        my.gen = gen,
        flag = 1,
        n.obj=n.obj
      )
    # plot(out$my.data2[,1:2])
    if (name == "de") {
      de_hv.plot <- data.frame(out$nfe, out$hv, "None")
      names(de_hv.plot) <- c("Evaluations", "HV", "Priority.Function")
      de_igd.plot <- data.frame(out$nfe, out$igd, "None")
      names(de_igd.plot) <- c("Evaluations", "IGD", "Priority.Function")
    }
    else if (name == "rad") {
      rad_hv.plot <- data.frame(out$nfe, out$hv, "MRDL")
      names(rad_hv.plot) <-
        c("Evaluations", "HV", "Priority.Function")
      rad_igd.plot <- data.frame(out$nfe, out$igd, "MRDL")
      names(rad_igd.plot) <- c("Evaluations", "IGD", "Priority.Function")
    }
    else if (name == "norm") {
      norm_hv.plot <- data.frame(out$nfe, out$hv, "Norm")
      names(norm_hv.plot) <-
        c("Evaluations", "HV", "Priority.Function")
      norm_igd.plot <- data.frame(out$nfe, out$igd, "Norm")
      names(norm_igd.plot) <- c("Evaluations", "IGD", "Priority.Function")
    }
    else if (name == "gra") {
      gra_hv.plot <- data.frame(out$nfe, out$hv, "R.I.")
      names(gra_hv.plot) <-
        c("Evaluations", "HV", "Priority.Function")
      gra_igd.plot <- data.frame(out$nfe, out$igd, "R.I.")
      names(gra_igd.plot) <- c("Evaluations", "IGD", "Priority.Function")
    }
    else if (name == "random") {
      random_hv.plot <- data.frame(out$nfe, out$hv, "Random")
      names(random_hv.plot) <-
        c("Evaluations", "HV", "Priority.Function")
      random_igd.plot <- data.frame(out$nfe, out$igd, "Random")
      names(random_igd.plot) <- c("Evaluations", "IGD", "Priority.Function")
    }
    
  }
  
  df2 <-
    rbind(
      data.frame(rad_hv.plot),
      data.frame(de_hv.plot),
      data.frame(norm_hv.plot),
      data.frame(gra_hv.plot),
      data.frame(random_hv.plot)
    )
  
  df3 <-
    rbind(
      data.frame(rad_igd.plot),
      data.frame(de_igd.plot),
      data.frame(norm_igd.plot),
      data.frame(gra_igd.plot),
      data.frame(random_igd.plot)
    )
  
  pathname <- paste0("../files/", fun, "hv_all.png")
  p2 <-
    ggplot(df2, aes(Evaluations, HV, group = Priority.Function)) +
    geom_line(aes(color = Priority.Function)) #+
  # geom_point(aes(shape = name, color = name))#+
  p2
  ggsave(filename = pathname, device = "png")
  
  
  pathname <- paste0("../files/", fun, "igd_all.png")
  p3 <- ggplot(df3, aes(Evaluations, IGD, group = Priority.Function)) +
    geom_line(aes(color = Priority.Function)) #+
  # geom_point(aes(shape = name, color = name))#+
  p3
  ggsave(filename = pathname, device = "png")
  
  create_graphs(df, fun.names, 2)
  
  write_feather(df, paste0("forboxplot_", fun))
  write_feather(df2, paste0("HV_gens_", fun))
  write_feather(df3, paste0("IGD_gens_", fun))
  
  variants <- c("de", "norm", "rad", "random", "gra")
}


# 
# 
# n.obj <- 3
# for (fun in fun.names) {
#   print(fun)
#   # things
#   objsColnames <- paste("#obj", 1:nObj, sep = "")
#   varsColnames <- paste("#var", 1:nVar, sep = "")
#   consColnames <- paste("#con", 1:nCon, sep = "")
#   
#   
#   
#   max.val <- c(-Inf, -Inf)
#   min.val <- c(Inf, Inf)
#   id <- 1
#   #loop for getting max/min values
#   print("first variant for")
#   for (variant in variants) {
#     runIdPre <- paste0("../", variant)
#     # temp <- read_feather(paste0("../",variant, "/UF1_info"))
#     temp <- read_feather(paste0("../", variant, "/", fun,"_",n.obj, "_info"))
#     temp <- as.data.frame(temp)
#     for (iRun2 in 1:nRun) {
#       iRun <- iRun2 - 1
#       
#       gen <- as.integer(temp[iRun2, ])
#       my.data <-
#         read.data(
#           fun = fun,
#           runIdPre = runIdPre,
#           iRun = iRun,
#           my.gen = gen,
#           flag = 0,
#           n.obj=n.obj
#         )$my.data2[, 1:2]
#       max.val <- pmax(max.val, apply(my.data, 2, max))
#       min.val <- pmin(min.val, apply(my.data, 2, min))
#     }
#   }
#   
#   de_median_gen <- data.frame()
#   gra_median_gen <- data.frame()
#   random_median_gen <- data.frame()
#   rad_median_gen <- data.frame()
#   norm_median_gen <- data.frame()
#   
#   de_median_hvs <- data.frame()
#   gra_median_hvs <- data.frame()
#   random_median_hvs <- data.frame()
#   rad_median_hvs <- data.frame()
#   norm_median_hvs <- data.frame()
#   
#   de_median_igds <- data.frame()
#   gra_median_igds <- data.frame()
#   random_median_igds <- data.frame()
#   rad_median_igds <- data.frame()
#   norm_median_igds <- data.frame()
#   
#   Yref <-
#     as.matrix(read.table(paste0("inst/extdata/pf_data/", fun, ".dat")))
#   #loop for getting scaled data, HV/IGD values over evaluations for all iteractions
#   print("2nd variant for")
#   for (variant in variants) {
#     # init with zero
#     print(variant)
#     initialValueM <- 0
#     initialValue <<- initialValueM
#     indicatorArc <<- matrix(initialValue, nrow = 1)
#     indicatorTmp <<- matrix(initialValue, nrow = nRun)
#     indicatorArcIGD <<- matrix(initialValue, nrow = nRun)
#     
#     times <- list()
#     ndom <- list()
#     fes <- list()
#     runIdPre <- paste0("../", variant)
#     df <- data.frame()
#     df2 <- data.frame()
#     # temp <- read_feather(paste0("../",variant, "/UF1_info"))
#     temp <- read_feather(paste0("../", variant, "/", fun,"_",n.obj, "_info"))
#     temp <- as.data.frame(temp)
#     for (iRun2 in 1:nRun) {
#       iRun <- iRun2 - 1
#       
#       
#       gen <- as.integer(temp[iRun2, ])
#       
#       out <-
#         read.data(
#           fun = fun,
#           runIdPre = runIdPre,
#           iRun = iRun,
#           my.gen = gen,
#           flag = 2,
#           n.obj=n.obj
#         )
#       my.data <- out$my.data2
#       
#       if (variant == "de") {
#         de_median_hvs <-
#           rbind(de_median_hvs, data.frame(out$hv, rep(1:gen), out$nfe))
#         de_median_igds <-
#           rbind(de_median_igds, data.frame(out$igd, rep(1:gen), out$nfe))
#       }
#       else if (variant == "rad") {
#         rad_median_hvs <-
#           rbind(rad_median_hvs, data.frame(out$hv, rep(1:gen), out$nfe))
#         rad_median_igds <-
#           rbind(rad_median_igds, data.frame(out$igd, rep(1:gen), out$nfe))
#       }
#       else if (variant == "gra") {
#         gra_median_hvs <-
#           rbind(gra_median_hvs, data.frame(out$hv, rep(1:gen), out$nfe))
#         gra_median_igds <-
#           rbind(gra_median_igds, data.frame(out$igd, rep(1:gen), out$nfe))
#       }
#       else if (variant == "norm") {
#         norm_median_hvs <-
#           rbind(norm_median_hvs, data.frame(out$hv, rep(1:gen), out$nfe))
#         norm_median_igds <-
#           rbind(norm_median_igds, data.frame(out$igd, rep(1:gen), out$nfe))
#       }
#       else if (variant == "random") {
#         random_median_hvs <-
#           rbind(random_median_hvs, data.frame(out$hv, rep(1:gen), out$nfe))
#         random_median_igds <-
#           rbind(random_median_igds,
#                 data.frame(out$igd, rep(1:gen), out$nfe))
#       }
#       
#       isFeasible <- extractFeasible(my.data)
#       cat("\n", sum(isFeasible), " solutions are feasible\n", sep = "")
#       
#       isFoundFeasible <<- T
#       objsTmp <- extractNonDominatedSolutions(my.data, isFeasible)
#       objsTmp2 <- convertEvalObjectives(objsTmp)
#       rankFeasible <- rankUnion(objsTmp2, T) #T or F???
#       archive <<- subset(archive, rankFeasible == 1)
#       cat(sum(rankFeasible == 1),
#           " solutions are nondominated\n",
#           sep = "")
#       
#       calculateIndicator(iRun2, objsTmp2, refPoint)
#       if (is.null(archive))
#         archive <- objsTmp2
#       # indicatorArcIGD[iRun2] <- calcIGD(archive[, 1:2], Yref)
#       pdGen <- formatC(gen,
#                        width = 3,
#                        format = "d",
#                        flag = "0")
#       times[[length(times) + 1]] <-
#         as.integer(read_feather(paste0(
#           runIdPre, "/", fun, "_time_", iRun + 1, "_", pdGen
#         )))
#       fes[[length(fes) + 1]] <- sum(isFeasible)
#       ndom[[length(ndom) + 1]] <- sum(rankFeasible == 1)
#     }
#     if (variant == "de") {
#       de_median_gen <-
#         cbind(
#           indicatorTmp,
#           indicatorArcIGD,
#           fes,
#           ndom,
#           times,
#           1:length(indicatorArcIGD),
#           "None"
#         )
#       colnames(de_median_gen) <-
#         c("HV",
#           "IGD",
#           "fesiable",
#           "nondominated",
#           "time",
#           "rep",
#           "name")
#     }
#     else if (variant == "rad") {
#       rad_median_gen <-
#         cbind(
#           indicatorTmp,
#           indicatorArcIGD,
#           fes,
#           ndom,
#           times,
#           1:length(indicatorArcIGD),
#           "MRDL"
#         )
#       colnames(rad_median_gen) <-
#         c("HV",
#           "IGD",
#           "fesiable",
#           "nondominated",
#           "time",
#           "rep",
#           "name")
#     }
#     else if (variant == "gra") {
#       gra_median_gen <-
#         cbind(
#           indicatorTmp,
#           indicatorArcIGD,
#           fes,
#           ndom,
#           times,
#           1:length(indicatorArcIGD),
#           "R.I."
#         )
#       colnames(gra_median_gen) <-
#         c("HV",
#           "IGD",
#           "fesiable",
#           "nondominated",
#           "time",
#           "rep",
#           "name")
#     }
#     else if (variant == "norm") {
#       norm_median_gen <-
#         cbind(
#           indicatorTmp,
#           indicatorArcIGD,
#           fes,
#           ndom,
#           times,
#           1:length(indicatorArcIGD),
#           "Norm"
#         )
#       colnames(norm_median_gen) <-
#         c("HV",
#           "IGD",
#           "fesiable",
#           "nondominated",
#           "time",
#           "rep",
#           "name")
#     }
#     else if (variant == "random") {
#       random_median_gen <-
#         cbind(
#           indicatorTmp,
#           indicatorArcIGD,
#           fes,
#           ndom,
#           times,
#           1:length(indicatorArcIGD),
#           "Random"
#         )
#       colnames(random_median_gen) <-
#         c("HV",
#           "IGD",
#           "fesiable",
#           "nondominated",
#           "time",
#           "rep",
#           "name")
#     }
#   }
#   df <-
#     rbind(
#       df,
#       data.frame(de_median_gen),
#       data.frame(rad_median_gen),
#       data.frame(gra_median_gen),
#       data.frame(random_median_gen),
#       data.frame(norm_median_gen)
#     )
#   df$fun <- fun
#   df$HV <- as.numeric(unlist(df$HV))
#   df$IGD <- as.numeric(unlist(df$IGD))
#   df$HV <- round(df$HV, 4)
#   df$IGD <- round(df$IGD, 4)
#   df$algorithm <- unlist(df$name)
#   
#   df$fesiable <- as.numeric(df$fesiable)
#   df$nondominated <- as.numeric(df$nondominated)
#   df$time <- as.numeric(df$time)
#   df$rep <- as.numeric(df$rep)
#   df$name <- as.character(df$name)
#   
#   #HV/IGD values over evaluations for "median" iteraction
#   print("last variant for")
#   variants<- c("None", "MRDL", "Random", "R.I.", "Norm")
#   # variants<- c("None")
#   for (variant in variants) {
#     none.median <-
#       df[which(df[df$algorithm == variant, ]$HV == median(df[df$algorithm == variant, ]$HV)), ]
#     
#     cat(variant, ": ",none.median$rep)
#     
#     if (variant == "None") name = "de"
#     else if (variant == "MRDL") name = "rad"
#     else if (variant == "R.I.") name = "gra"
#     else if (variant == "Random") name = "random"
#     else if (variant == "Norm") name = "norm"
#     
#     runIdPre <- paste0("../", name)
#     temp <- read_feather(paste0("../", name, "/", fun, "_info"))
#     temp <- as.data.frame(temp)
#     
#     iRun2 <- none.median$rep[1]
#     iRun <- iRun2 - 1
#     gen <- as.integer(temp[iRun2, ])
#     
#     out <-
#       read.data(
#         fun = fun,
#         runIdPre = runIdPre,
#         iRun = iRun,
#         my.gen = gen,
#         flag = 1,
#         n.obj=n.obj
#       )
#     # plot(out$my.data2[,1:2])
#     if (name == "de") {
#       de_hv.plot <- data.frame(out$nfe, out$hv, "None")
#       names(de_hv.plot) <- c("Evaluations", "HV", "Priority.Function")
#       de_igd.plot <- data.frame(out$nfe, out$igd, "None")
#       names(de_igd.plot) <- c("Evaluations", "IGD", "Priority.Function")
#     }
#     else if (name == "rad") {
#       rad_hv.plot <- data.frame(out$nfe, out$hv, "MRDL")
#       names(rad_hv.plot) <-
#         c("Evaluations", "HV", "Priority.Function")
#       rad_igd.plot <- data.frame(out$nfe, out$igd, "MRDL")
#       names(rad_igd.plot) <- c("Evaluations", "IGD", "Priority.Function")
#     }
#     else if (name == "norm") {
#       norm_hv.plot <- data.frame(out$nfe, out$hv, "Norm")
#       names(norm_hv.plot) <-
#         c("Evaluations", "HV", "Priority.Function")
#       norm_igd.plot <- data.frame(out$nfe, out$igd, "Norm")
#       names(norm_igd.plot) <- c("Evaluations", "IGD", "Priority.Function")
#     }
#     else if (name == "gra") {
#       gra_hv.plot <- data.frame(out$nfe, out$hv, "R.I.")
#       names(gra_hv.plot) <-
#         c("Evaluations", "HV", "Priority.Function")
#       gra_igd.plot <- data.frame(out$nfe, out$igd, "R.I.")
#       names(gra_igd.plot) <- c("Evaluations", "IGD", "Priority.Function")
#     }
#     else if (name == "random") {
#       random_hv.plot <- data.frame(out$nfe, out$hv, "Random")
#       names(random_hv.plot) <-
#         c("Evaluations", "HV", "Priority.Function")
#       random_igd.plot <- data.frame(out$nfe, out$igd, "Random")
#       names(random_igd.plot) <- c("Evaluations", "IGD", "Priority.Function")
#     }
#     
#   }
#   
#   df2 <-
#     rbind(
#       data.frame(rad_hv.plot),
#       data.frame(de_hv.plot),
#       data.frame(norm_hv.plot),
#       data.frame(gra_hv.plot),
#       data.frame(random_hv.plot)
#     )
#   
#   df3 <-
#     rbind(
#       data.frame(rad_igd.plot),
#       data.frame(de_igd.plot),
#       data.frame(norm_igd.plot),
#       data.frame(gra_igd.plot),
#       data.frame(random_igd.plot)
#     )
#   
#   pathname <- paste0("../files/", fun, "hv_all.png")
#   p2 <-
#     ggplot(df2, aes(Evaluations, HV, group = Priority.Function)) +
#     geom_line(aes(color = Priority.Function)) #+
#   # geom_point(aes(shape = name, color = name))#+
#   p2
#   ggsave(filename = pathname, device = "png")
#   
#   
#   pathname <- paste0("../files/", fun, "igd_all.png")
#   p3 <- ggplot(df3, aes(Evaluations, IGD, group = Priority.Function)) +
#     geom_line(aes(color = Priority.Function)) #+
#   # geom_point(aes(shape = name, color = name))#+
#   p3
#   ggsave(filename = pathname, device = "png")
#   
#   create_graphs(df, fun.names, 2)
#   
#   write_feather(df, paste0("forboxplot_", fun,"_",n.obj,"_"))
#   write_feather(df2, paste0("HV_gens_", fun,"_",n.obj,"_"))
#   write_feather(df3, paste0("IGD_gens_", fun,"_",n.obj,"_"))
#   
#   variants <- c("de", "norm", "rad", "random", "gra")
# }
# 
