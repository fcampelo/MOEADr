library(feather)
library(MOEADr)
library(withr)
library(stringr)
library(emoa)
library(ggplot2)


extractFeasible <- function(data, isFilteredByCons = T) {
  nr1 <- nrow(data)
  isFeasible <- rep(T, nr1)
  if (isFilteredByCons) {
    stopifnot(length(consColnames) == nCon)
    for (i in 1:nCon) {
      fPart <- ifelse(data[, nObj + i] >= 0, T, F)
      isFeasible <- isFeasible & fPart
    }
  } else {
    cat("no filtering by constraints\n")
    flush.console()
  }
  return (isFeasible)
}

extractNonDominatedSolutions <- function(data, isFeasible) {
  idxFeasible <- ifelse(isFeasible == T, 1:nrow(data), 0)
  idxFeasible <- subset(idxFeasible, idxFeasible > 0)
  #objsTmp <<- as.matrix(data[idxFeasible, 1:(2 + nObj)]) # for output
  objsTmp <- as.matrix(data[idxFeasible, 1:(nObj + nCon)]) # for output
  objsTmp
}


rankUnion <- function(dataTmp, isRedefineArc) {
  # Re-define Archive arrays for write history
  if (isRedefineArc == T) {
    # initialize archive and store dataTmp as it is 
    archive <<-  as.matrix(dataTmp)
    archive <<- as.matrix(archive[complete.cases(archive[, 1]), ])
  } else {
    # achive stores only objectives
    archive <<- as.matrix(rbind(archive[, 1:(nObj + nCon)], dataTmp[, 1:(nObj + nCon)]))
    archive <<- as.matrix(archive[complete.cases(archive[, 1]), ])
  }
  #
  if (ncol(archive) == 1) {
    archive <<- t(archive)
  }
  #
  if (nrow(archive) == 1) {
    idd <- as.matrix(F)
  } else {
    idd <- as.matrix(is_dominated(t(as.matrix(archive[, 1:nObj]))))
  }
  #
  rankFeasible <- ifelse(idd == F, 1, 2)
  return (rankFeasible)
}


convertEvalObjectives <- function(objsTmp) {
  conversionEvalString <- paste("",
                                "dataTmp[, 1] <- objsTmp[, 1];",
                                "dataTmp[, 2] <- objsTmp[, 2];",
                                "dataTmp[, 3] <- objsTmp[, 3];",
                                "")
  if (ncol(objsTmp) == 1) {
    objsTmp <<- t(objsTmp)
  }
  dataTmp <- objsTmp
  #
  eval(parse(text = conversionEvalString))
  #
  return (dataTmp)
}


calculateIndicator <- function(iRun2, dataTmp, refPoint) {
  if (nrow(dataTmp) > 0) {
    if (nrow(dataTmp) == 1) {
      indicatorTmp[iRun2] <<- emoa::dominated_hypervolume((as.matrix(dataTmp[, 1:nObj])), t(as.matrix(refPoint)))
    } else {
      indicatorTmp[iRun2] <<- emoa::dominated_hypervolume(t(as.matrix(dataTmp[, 1:nObj])), t(as.matrix(refPoint)))
    }
  } else {
    indicatorTmp[iRun2]  <<- initialValue
  }
  if (nrow(archive) > 0) {
    if (nrow(archive) == 1) {
    } else {
      indicatorArc[iRun2] <<- emoa::dominated_hypervolume(t(as.matrix(archive[, 1:nObj])), t(as.matrix(refPoint)))
    }
  }
}

calculateIndicator <- function(iRun2, dataTmp, refPoint) {
  if (nrow(dataTmp) > 0) {
    if (nrow(dataTmp) == 1) {
      indicatorTmp[iRun2] <<- emoa::dominated_hypervolume((as.matrix(dataTmp[, 1:nObj])), t(as.matrix(refPoint)))
    } else {
      indicatorTmp[iRun2] <<- emoa::dominated_hypervolume(t(as.matrix(dataTmp[, 1:nObj])), t(as.matrix(refPoint)))
    }
  } else {
    indicatorTmp[iRun2]  <<- initialValue
  }
  if (nrow(archive) > 0) {
    if (nrow(archive) == 1) {
    } else {
      indicatorArc[iRun2] <<- emoa::dominated_hypervolume(t(as.matrix(archive[, 1:nObj])), t(as.matrix(refPoint)))
    }
  }
}

read.data <- function(fun, runIdPre, iRun, my.gen, flag = 0, n.obj=NULL){
  runDig <- 3
  generationDig <- 3
  nr1prev <- 0
  # cat("gen = ")
  # print(runIdPre)
  hv <- matrix(0, nrow = gen)
  igd <- matrix(0, nrow = gen)
  nfe <- matrix(0, nrow = gen)
  my.data2 <- data.frame()
  for (i in 1:my.gen){
    iGen2 <- i
    iGen <- i - 1
    # cat(iGen, ", ")
    filePath <- fun
    # runIdPre <- ""
    runIdPost <- ""
    objsFilePre <- ""
    varsFilePre <- ""
    consFilePre <- ""
    objsFilePost <- ""
    varsFilePost <- ""
    consFilePost <- ""

    # zpdRun <- formatC(iRun, width = runDig, format = "d", flag = "0") #zero padded
    zpdGen <- formatC(iGen2, width = generationDig, format = "d", flag = "0") #zero padded
    # tgt <- paste(filePath, runIdPre, zpdRun, runIdPost, objsFilePre, zpdGen, objsFilePost, sep = "")
    # if (is.null(n.obj)) 
      tgt <- paste0(runIdPre,"/", fun,"_rep_",iRun2,"_",zpdGen,"_Y")
    # else tgt <- paste0(runIdPre,"/", fun,"_",n.obj,"_", "_rep_",iRun2,"_",zpdGen,"_Y")
    # objsData <- read.table(tgt, header = F, sep = "\t")
    objsData <- as.matrix(read_feather(tgt))
    # print(objsData[,1:nObj])
    # print(objsData[,1:nObj])
    # exit()
    # tgt <- paste(filePath, runIdPre, zpdRun, runIdPost, varsFilePre, zpdGen, varsFilePost, sep = "")
    # varsData <- read.table(tgt, header = F, sep = "\t")
    # tgt <- paste(filePath, runIdPre, zpdRun, runIdPost, consFilePre, zpdGen, consFilePost, sep = "")
    # consData <- read.table(tgt, header = F, sep = "\t")
    nr1 <- nrow(objsData)
    gen <- rep(iGen2, each = nr1)
    evals <- rep((nr1prev + 1):(nr1prev + nPop+1), length = nr1)
    zeroObj <- matrix(0.0, nrow=dim(objsData)[1], ncol=1 )
    # temp <- data.frame(objsData[, 1:nObj], consData, varsData, gen, evals)
    # temp <- data.frame(objsData[, 1:nObj], gen, evals)
    # colnames(temp) <- c(objsColnames, consColnames, varsColnames, "#Gen", "#Eval")
    # my.data2 <- rbind(my.data, data.frame(objsData[, 1:nObj], gen, evals))
    
    # UF1_iter_nfe_1_001
    # if (is.null(n.obj)) 
      tgt <- paste0(runIdPre,"/", fun,"_iter_nfe_",iRun2,"_",zpdGen)
    # else tgt <- paste0(runIdPre,"/", fun,"_",n.obj,"_","_iter_nfe_",iRun2,"_",zpdGen)
    # objsData <- read.table(tgt, header = F, sep = "\t")
    temp <- as.matrix(read_feather(tgt))
    if (i > 1) nfe[i,1] <- temp[,2] + nfe[i-1,1]  
    else nfe[i,1] <- temp[,2]
    
    consData <- rep(0, nr1)
    varsData <- rep(0, nr1)
    # print(evals)
    
    nr1prev <<- nr1prev + nPop
    if (flag == 1){
      # objsData[, 1:nObj] <- (sweep(objsData[, 1:nObj], 2, abs(min.val), "+"))
      if (fun != "moon") {
        igd[i,1] <- calcIGD(objsData[, 1:nObj], Yref)
        a<-(sweep(objsData[, 1:nObj], 2, min.val))
        b<-((max.val - min.val) + 1e-50)
        objsData[, 1:nObj] <- sweep(a, 2, b, "/")
      }
      hv[i,1] <- emoa::dominated_hypervolume(t(as.matrix(objsData[,1:nObj])), t(as.matrix(refPoint)))
    }
    if (i == my.gen && flag == 2){
     
      # objsData[, 1:nObj] <- objsData[, 1:nObj] + abs(min.val)
      if (fun != "moon") {
        igd[i,1] <- calcIGD(objsData[, 1:nObj], Yref)
        a<-(sweep(objsData[, 1:nObj], 2, min.val))
        b<-((max.val - min.val) + 1e-50)
        objsData[, 1:nObj] <- sweep(a, 2, b, "/")
      }
         
      hv[i,1] <- emoa::dominated_hypervolume(t(as.matrix(objsData[,1:nObj])), t(as.matrix(refPoint)))
    }
  }
  my.data2 <- rbind(my.data2, data.frame(objsData[, 1:nObj], gen, evals, consData, varsData))    # my.data2 <- rbind(my.data, data.frame(objsData[, 1:nObj], consData, varsData, gen, evals))
  out <- list(my.data2 = my.data2, hv = hv, igd = igd, nfe = nfe)
  return (out)
}


calc_hv <- function(data, variation, fun, repetitions, max.val, min.val, ref.points = c(1,1), epsilon = 1e-50) {
  moead.data <- list()
  data <- (sweep(data, 2, min.val))/ ((max.val - min.val) + epsilon)
  non.d <- find_nondominated_points(data)
  if (sum(non.d) > 0){
  moead.hv <-
    dominated_hypervolume(points = t(data[non.d,]),
                                ref = ref.points)
  }
  else moead.hv <- 0
  temp <- data.frame(moead.hv, fun, paste0(variation), repetitions-1)
  
  colnames(temp) <-
    c("HV",
      "fun",
      "algorithm", "rep")
  temp
}

create_graphs <-
  function(my.data,
           fun.names,
           n.obj) {
    # algorithm <- "moead.de"
    for (fun in fun.names) {
      # my.data <- data[data$fun == fun, ]
      # my.data$algorithm <- factor(my.data$algorithm,levels = c("MRDL", "None", "Spectral-Norm", "R.I.", "Random"))
      pathname <- paste0("../files/", fun, "_HV.png")
      p <- ggplot(my.data, aes(algorithm, HV)) +geom_boxplot(aes(fill = algorithm), scale = "count")
        # geom_violin(aes(fill = algorithm), scale = "count") #+ ylim(0, 1) +
      # geom_violin(aes(fill = algorithm)) +
      ggtitle(fun) 
      p <-
        p + geom_boxplot(width = 0.4, alpha = 0.65) + theme(axis.text = element_text(size =
                                                                                       14),
                                                            axis.title =
                                                              element_text(size = 16, face = "bold")) + theme(plot.title = element_text(
                                                                color = "blue",
                                                                size = 24,
                                                                face = "bold"
                                                              )) + geom_jitter(height = 0, width = 0.1)
      p +labs(title=paste0("HV - ",fun),
                x ="Priority Function")
      ggsave(filename = pathname, device = "png")
      
      pathname <- paste0("../files/", fun, "_IGD.png")
      p <- ggplot(my.data, aes(algorithm, IGD)) +geom_boxplot(aes(fill = algorithm), scale = "count")
        # geom_violin(aes(fill = algorithm), scale = "count") #+ ylim(0, 1) +
      # geom_violin(aes(fill = algorithm)) +
      ggtitle(fun) 
      p <-
        p + geom_boxplot(width = 0.4, alpha = 0.65) + theme(axis.text = element_text(size =
                                                                                       14),
                                                            axis.title =
                                                              element_text(size = 16, face = "bold")) + theme(plot.title = element_text(
                                                                color = "blue",
                                                                size = 24,
                                                                face = "bold"
                                                              )) + geom_jitter(height = 0, width = 0.1)
      p+labs(title=paste0("IGD - ",fun),
             x ="Priority Function")
      ggsave(filename = pathname, device = "png")
    }
  }

