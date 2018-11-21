# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # post_moon.r_
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #  Author:
# #       Hiroaki FUKUMOTO <fukumoto@flab.isas.jaxa.jp>
# #       Takehisa KOHIRA  <kohira.t@mazda.co.jp>
# #       Tomoaki TATSUKAWA <tatsukawa@rs.tus.ac.jp>
# #
# #  Copyright (c) 2018 Tomoaki TATSUKAWA, Takehisa KOHIRA, and Hiroaki FUKUMOTO
# #
# #  This program is free software: you can redistribute it and/or modify
# #  it under the terms of the GNU Lesser General Public License as published
# #  by the Free Software Foundation, either version 2 of the License.
# #
# #  This program is WITHOUT ANY WARRANTY.
# #  See the GNU Lesser General Public License for more details.
# #
# #  You should have received a copy of the GNU Lesser General Public License
# #  along with this program.  If not, see <http://www.gnu.org/licenses/>.
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# remove all objects
rm(list = ls())
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #  Parameters   # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # Working Directory # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# specify working directory.
# file paths will be of relative from this directory
#setwd('D:/Data/work/benchmark')
#setwd("/home/foo/work/benchmark")

# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # Your Group ID # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
grp <- "LabEC"

# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # Single or Multi Objective Optimization  # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#sngl <- T #single objective
sngl <- F #multi objective

# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # Population Size and Generation Size # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# population size
nPop <- 153
# number of generations
# de
# nGen <- rep(392, 21)
# rad
nGen <- rep(1066, 21)
# nGen <- rep(389, 21)
# nGen[1] <- 1066
# nGen[2] <- 389
# nGen[3] <- 389
# nGen[4] <- 389
# nGen[5] <- 389
# nGen[6] <- 389
# nGen[7] <- 392
# nGen[8] <- 389
# nGen[9] <- 392
# nGen[10] <- 391
# nGen[11] <- 390
# nGen[12] <- 392
# nGen[13] <- 392
# nGen[14] <- 390
# nGen[15] <- 392
# nGen[16] <- 388
# nGen[17] <- 388
# nGen[18] <- 391
# nGen[19] <- 391
# nGen[20] <- 392
# nGen[21] <- 389
# gra
# nGen <- rep(Inf, 21)
# nGen[1] <- 301
# nGen[2] <- 969
# nGen[3] <- 1039
# nGen[4] <- 1045
# nGen[5] <- 1248
# nGen[6] <- 935
# nGen[7] <- 1028
# nGen[8] <- 974
# nGen[9] <- 889
# nGen[10] <- 989
# nGen[11] <- 864
# nGen[12] <- 1061
# nGen[13] <- 992
# nGen[14] <- 1075
# nGen[15] <- 674
# nGen[16] <- 1025
# nGen[17] <- 892
# nGen[18] <- 1084
# nGen[19] <- 1101
# nGen[20] <- 803
# nGen[21] <- 955
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # Number Runs # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# number of trials
nRun <- 21

# nGen <- rep(330, 21)
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # File Directories  # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # CASE 1: result file for a run is in one file
# # example:
# #  isSoleFile <- T
# #  nrun <- 21
# #  ngen <- 100
# #  filepath <- "./interface/"
# #  runidpre <-  "history"
# #  runidpost <-  "_th.txt"
# #  runiddigit <- 3
# #  then file name will be:
# #    ./interface/history_000th.txt
# #  ~ ./interface/history_020th.txt
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # CASE 2: result file for a run is divided by generation
# # example:
# #  isSoleFile <- F
# #  nrun <- 21
# #  ngen <- 100
# #  filepath <- "./interface/"
# #  runidpre <- "work_"
# #  runidpost <- "th/"
# #  varsfilepre <- "gen"
# #  varsfilepost <- "_pop_vars_eval.txt"
# #  generationdig <- 4
# #  runiddigit <- 3
# #  then file name will be:
# #    ./interface/work_000th/gen0000_pop_vars_eval.txt
# #  ~ ./interface/work_000th/gen0099_pop_vars_eval.txt
# #  ~ ./interface/work_020th/gen0000_pop_vars_eval.txt
# #  ~ ./interface/work_020th/gen0099_pop_vars_eval.txt
# # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # CASE 1:
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#NOT SUPPOERTED in JPNSEC COMPETITION 2018#
#isSoleFile <- T
## path is of relative from the working directory
#filePath <- "./interface/"
##
#runIdPre<- "history_"
#runIdPost<- "th.txt"
##
#runDig <- 3
##

# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # CASE 2:
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
isSoleFile <- F
# path is of relative from the working directory
filePath <- "./interface/"
#
runIdPre<- "work_"
# runIdPost<- "th/" # include slash if needed
#
#
#//TESTRUN DIR
filePath <- "./testrun_mop/"
filePath <- "./"
#filePath <- "./testrun_sop/"
runIdPre<- ""
runIdPost<- "th_run/optimizer/interface/" # include slash if needed
#TESTRUN DIR//
#
varsFilePre <- "gen"
objsFilePre <- "gen"
consFilePre <- "gen"
#
varsFilePost <- "_pop_vars_eval.txt"
objsFilePost <- "_pop_objs_eval.txt"
consFilePost <- "_pop_cons_eval.txt"
#
generationDig <- 4
runDig <- 3
#


# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # #
# Problem # # # #
# # # # # # # # #
# number of objectives
nObj <- 3 # fix even if single obj 
# number of variables
nVar <- 2
# number of constraints
nCon <- 2



# # # # # # # # #
# Headerfiles # #
# # # # # # # # #
objsHeaderFile <- "objs_header.csv"
varsHeaderFile <- "vars_header.csv"
consHeaderFile <- "cons_header.csv"

# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # Other Misc Options  # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# extract non-dominated solutions for:
#   0: all generations at once,
#   1: each generation
#   default option is 0
paretoSel <- 0
#paretoSel <- 1
#
# whether if use header file; True or False
# default is F
useHeaderFile <- F
#
# whether if filter results with constraint violation
# default is T
isFilteredByCons <- T
#
# whether if write prt and history file
# default is T
isWritePrtFile <- T
isWriteHistoryFile <- T
#
# starting number of generation count and run id in filename
firstRun <- 0
firstGen <- 0
#
# reference point for HV calculation
refPoint <- matrix(c(1.0, 0.0, 1.0), nrow = 1, ncol = nObj)
#
# initial indicator value 
initialValueS <- Inf # for weight indicator
initialValueM <- 0 # for HV indicator
#
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
# objective conversion expression. this string will be 
# evaluated using eval
# NOTE1:
#   dataTmp[, 1] is for obj. 1
#   dataTmp[, 2] is for obj. 2
#   dataTmp[, 3] is for obj. 3
# NOTE2:
#   archive should be used with super-assignment operator ( <<- )
# # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
##no conversion is required by default
conversionEvalString <- paste("",
    "dataTmp[, 1] <- objsTmp[, 1];",
    "dataTmp[, 2] <- objsTmp[, 2];",
    "dataTmp[, 3] <- objsTmp[, 3];",
"")
### 
conversionOutputString <- NULL
#
#
ndigits <- 10


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #  Preprocess   # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # #
# Package # # # #
# # # # # # # # #
# read package 'emoa'

if (!require(emoa)) {
    # if not installed, try to install
    options(repos="http://cran.md.tsukuba.ac.jp")
    install.packages("emoa")
    # stop if yet failing to load emoa
    stopifnot(require(emoa))
}

# # # # # # # # #
# Outputfiles # #
# # # # # # # # #
# outfiles will be in the working directory
outPre <- ifelse(sngl == T, "s", "m")
outMed <- ifelse(sngl == T, "opt", "prt")
medianHistoryFile <- paste(outPre, "_his_", grp, ".csv", sep="") 
medianPrtFile <- paste(outPre, "_" , outMed, "_", grp, ".csv", sep="") 
bestHistoryFile <- paste(outPre, "_bst_his_", grp, ".csv", sep="") 
bestPrtFile <- paste(outPre, "_bst_", grp, ".csv", sep="") 
resultFile <- "result.txt"

# # # # # # # # #
# Headers # # # #
# # # # # # # # #
if (useHeaderFile) {
    # read header file
    objsInfo <- read.table(objsHeaderFile, header = F, sep = ",")
    varsInfo <- read.table(varsHeaderFile, header = F, sep = ",")
    consInfo <- read.table(consHeaderFile, header = F, sep = ",")
    # set header
    objsColnames <- as.character(objsInfo[, 1])
    varsColnames <- as.character(varsInfo[, 2])
    consColnames <- as.character(consInfo[, 1])
} else {
    cat("process without header file\n")
    flush.console()
    # set header
    objsColnames <- paste("#obj", 1:nObj, sep = "")
    varsColnames <- paste("#var", 1:nVar, sep = "")
    consColnames <- paste("#con", 1:nCon, sep = "")
}

# # # # # # # # #
# Indicator # # #
# # # # # # # # #
isFoundFeasible <- F
indicatorName <- ifelse(sngl == T, "Communication Time", "HyperVolume")
#
options(digits = ndigits)




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #  Subfunctions # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# initialize indicator array with initialValues
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
initializeIndicator <- function() {
    if (sngl == T) { #single
        initialValue <<- initialValueS
    } else {
        initialValue <<- initialValueM
    }
    indicatorArc <<- matrix(initialValue, nrow = nRun) 
    indicatorTmp <<- matrix(initialValue, nrow = nRun) 
    #
    if (sngl == T) {
        dataHistory <<- matrix(, nrow = 0, ncol = 6) 
    } else {
        dataHistory <<- matrix(, nrow = 0, ncol = 5) 
    }
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# subroutine for whole generation calculation
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
executeSel0 <- function(iRun, iRun2, verb, isWrite, fileName) {
    #
    #objsTmp2 <- matrix(, nrow = 0, ncol = 2 + nObj)
    objsTmp2 <- matrix(, nrow = 0, ncol = nObj + nCon)
    #
    #
    #
    # # # # # # # # # # # # # # # # # # # # # # # 
    # read data
    # # # # # # # # # # # # # # # # # # # # # # # 
    if (isSoleFile == F) {
        iGen <- firstGen
        iGen2 <- 1
        if(verb) cat("Gen = ")
        while (iGen2 <= nGen[iRun+1]) {
            if(verb) {
                if(iGen2 != nGen[iRun+1]) { cat(iGen, ", ") } else { cat(iGen) }
            }
            if(verb) flush.console()
            #
            # do not refresh data array each generation unlike paretoSel==1
            ##### data <<- matrix(, nrow = 0, ncol = nObj + nVar + nCon + 2)
            readData(iRun, iGen, iGen2)
            #
            iGen <- iGen + 1
            iGen2 <- iGen2 + 1
        }#Gen
        if(verb) cat("\n")
    } else {
        readData(iRun, iGen, iGen2)
    }
    #
    #
    #
    # # # # # # # # # # # # # # # # # # # # # # # 
    # extract feasible solutions from data array
    # # # # # # # # # # # # # # # # # # # # # # #
    isFeasible <- extractFeasible()
    if(verb) cat(sum(isFeasible), " solutions are feasible\n", sep = "")
    if(verb) flush.console()
    #
    #
    #
    # # # # # # # # # # # # # # # # # # # # # # # 
    # extract non-dominated solutions
    # and store them into archive array
    # # # # # # # # # # # # # # # # # # # # # # # 
    if (sum(isFeasible) > 0) {
        #
        isFoundFeasible <<- T
        #
        extractNonDominatedSolutions(isFeasible)
        #
        objsTmp2 <- convertEvalObjectives()
        #
        rankFeasible <- rankUnion(objsTmp2, F)
        archive <<- subset(archive, rankFeasible == 1)
        if(verb) cat(sum(rankFeasible == 1), " solutions are nondominated\n", sep = "")
        if(verb) flush.console()
        #
    }
    #
    #
    #
    # # # # # # # # # # # # # # # # # # # # # # # 
    # calculate indicator
    # # # # # # # # # # # # # # # # # # # # # # # 
    calculateIndicator(iRun2, objsTmp2)
    #
    if(verb) cat(indicatorName, " = ", indicatorArc[iRun2], "\n", sep = "")
    if(verb) flush.console()
    #
    #
    #
    # # # # # # # # # # # # # # # # # # # # # # # 
    # write arbit. run id data like pareto.csv
    # # # # # # # # # # # # # # # # # # # # # # # 
    if(isWrite == T) {
        data <<- subset(data, isFeasible ==1)
        if (nrow(data) > 0 ) {
            dataNull <- rankUnion(data, T)
            archive <<- subset(archive, rankFeasible == 1)
            convertOutObjectives()
            if (sngl == T) {
              #colnames(archive) <- c("#Gen", "Eval", objsColnames, varsColnames, consColnames)
              indices = c(2, 3)
              if (nrow(archive) == 1 ) {
                archive <<- t(as.matrix(archive[-indices]))
                colnames(archive) <- c(objsColnames[1], consColnames, varsColnames, "#Gen", "#Eval")
              } else {
                archive <<- archive[, -indices]
                colnames(archive) <- c(objsColnames[1], consColnames, varsColnames, "#Gen", "#Eval")
              }
            } else {
              colnames(archive) <- c(objsColnames, consColnames, varsColnames, "#Gen", "#Eval")
            }
            write.table(archive, fileName, c = T, r = F, quote = F, sep = ",")
            cat("[II] output ", fileName, "\n")
        } else {
            cat("[WW] this run has no feasible sollution\n")
        }
        flush.console()
    }
    # 
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# subroutine for each generation calculation
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
executeSel1 <- function(iRun, iRun2, verb, isWrite, fileName) {
    #
    if (isSoleFile == T) {
        data <<- matrix(, nrow = 0, ncol = nObj + nVar + nCon + 2)
        readData(iRun, iGen, iGen2)
        data2 <- data
    }
    iGen <- firstGen
    iGen2 <- 1
    while (iGen2 <= nGen[iRun+1]) {
        if(verb) cat("\nRun = ", iRun , " Gen = ", iGen , "\n", sep = "")
        if(verb) flush.console()
        #
        #objsTmp2 <- matrix(, nrow = 0, ncol = 2 + nObj)
        objsTmp2 <- matrix(, nrow = 0, ncol = nObj + nCon)
        #
        #
        #
        #
        # # # # # # # # # # # # # # # # # # # # # # # 
        # read data
        # # # # # # # # # # # # # # # # # # # # # # # 
        # refresh data array each generation
        if (isSoleFile == F) {
            data <<- matrix(, nrow = 0, ncol = nObj + nVar + nCon + 2)
            readData(iRun, iGen, iGen2)
        } else {
            #data <<- subset(data2, data2[, 1] == iGen2)
            data <<- subset(data2, data2[, nObj + nVar + nCon + 1] == iGen2)
        }
        #
        #
        #
        # # # # # # # # # # # # # # # # # # # # # # # 
        # extract feasible solutions from data array
        # # # # # # # # # # # # # # # # # # # # # # #
        isFeasible <- extractFeasible()
        if(verb) cat(sum(isFeasible), " solutions are feasible\n", sep = "")
        if(verb) flush.console()
        #
        #
        #
        # # # # # # # # # # # # # # # # # # # # # # # 
        # extract non-dominated solutions
        # and store them into archive array
        # # # # # # # # # # # # # # # # # # # # # # # 
        if (sum(isFeasible) > 0) {
            #
            isFoundFeasible <<- T
            #
            extractNonDominatedSolutions(isFeasible)
            #
            objsTmp2 <- convertEvalObjectives()
            #
            rankFeasible <- rankUnion(objsTmp2, F)
            archive <<- subset(archive, rankFeasible == 1)
            if(verb) cat(sum(rankFeasible == 1), " solutions are nondominated\n", sep = "")
            if(verb) flush.console()
            #
        }
        #
        #
        #
        # # # # # # # # # # # # # # # # # # # # # # # 
        # calculate indicator
        # # # # # # # # # # # # # # # # # # # # # # # 
        calculateIndicator(iRun2, objsTmp2)
        #
        if(verb) cat(indicatorName, " using only this generation = ", indicatorTmp[iRun2], "\n", sep = "")
        if(verb) cat(indicatorName, " using whole     generation = ", indicatorArc[iRun2], "\n", sep = "")
        if(verb) flush.console()
        #
        if (sngl == T) {
            if (nrow(archive) > 0) {
                dataHistory <<- rbind(dataHistory, t(as.matrix(c(as.integer(iGen2), as.integer(iGen2*nPop), as.matrix(archive)[1, 1], as.matrix(archive)[1, (nObj + 1):(nObj + nCon)], as.integer(sum(isFeasible))))))
            }
        } else {
            dataHistory <<- rbind(dataHistory, t(as.matrix(c(as.integer(iGen2), as.integer(iGen2*nPop), indicatorTmp[iRun2], indicatorArc[iRun2], as.integer(sum(isFeasible))))))
        }
        #
        iGen <- iGen + 1
        iGen2 <- iGen2 + 1
        #
    }#Gen
    #
    #
    #
    # # # # # # # # # # # # # # # # # # # # # # # 
    # write arbit. run id data like pareto.csv
    # # # # # # # # # # # # # # # # # # # # # # # 
    if (isWrite == T) {
        if (sngl == T) {
            colnames(dataHistory) <- c("#Gen", "#Eval", indicatorName, consColnames, "Num of feasible individuals")
        } else {
            colnames(dataHistory) <- c("#Gen", "#Eval", paste(indicatorName, " using each gen", sep=""), paste(indicatorName, " using whole gen", sep=""), "Num of feasible individuals")
        }
        write.table(dataHistory, fileName, c = T, r = F, quote = F, sep = ",")
        cat("[II] output ", fileName, "\n")
        flush.console()
    }
    #
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# read objectives ,variables, and constraints and and append them to data array
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
readData <- function(iRun, iGen, iGen2) {
    if (isSoleFile == T) {
        cat("reading input file...\n")
        zpdRun <- formatC(iRun, width = runDig, format = "d", flag = "0") #zero padded
        #
        tgt <- paste(filePath, "/", runIdPre, zpdRun, runIdPost, sep = "")
        data <<- read.table(tgt, header = F, sep = "\t")
        nr1 <- nrow(data)
        flonr0 <- nr1 / nPop
        flonr1 <- floor(flonr0)
        genby <- nPop * nGen[iRun+1]
        if (genby == nr1) {
          gen <- rep(1:nGen[iRun+1], each = nPop)
        } else {
          gen <- rep(1:flonr1, each = nPop)
          nTimes <- nPop * nGen[iRun+1] - nr1
          newnPop <- nPop * flonr1
          gen[newnPop : nr1] <- rep(nGen[iRun+1], times = nTimes)
        }
        evals <- rep(1:nPop, length = nr1)
        #concat gen, eval, objs, vars, and cons
        #data <<- data.frame(gen, evals, data)
        data <<- data.frame(data, gen, evals)
        data <<- as.matrix(data[, 1:(2 + nObj + nVar + nCon)])
    } else {
        zpdRun <- formatC(iRun, width = runDig, format = "d", flag = "0") #zero padded
        zpdGen <- formatC(iGen, width = generationDig, format = "d", flag = "0") #zero padded
        tgt <- paste(filePath, "/", runIdPre, zpdRun, runIdPost, objsFilePre, zpdGen, objsFilePost, sep = "")
        objsData <- read.table(tgt, header = F, sep = "\t")
        tgt <- paste(filePath, "/", runIdPre, zpdRun, runIdPost, varsFilePre, zpdGen, varsFilePost, sep = "")
        varsData <- read.table(tgt, header = F, sep = "\t")
        tgt <- paste(filePath, "/", runIdPre, zpdRun, runIdPost, consFilePre, zpdGen, consFilePost, sep = "")
        consData <- read.table(tgt, header = F, sep = "\t")
        nr1 <- nrow(objsData)
        gen <- rep(iGen2, each = nr1)
        evals <- rep((nr1prev + 1):(nr1prev + nPop), length = nr1) 
        #concat gen, eval, objs, vars, and cons
        #data <<- rbind(data, data.frame(gen, evals, objsData[, 1:nObj], varsData, consData))
        zeroObj <- matrix(0.0, nrow=dim(objsData)[1], ncol=1 )
        if (sngl == T) {
          data <<- rbind(data, data.frame(objsData[, 1], zeroObj, zeroObj, consData, varsData, gen, evals))
        } else {
          data <<- rbind(data, data.frame(objsData[, 1:nObj], consData, varsData, gen, evals))
        }
        nr1prev <<- nr1prev + nPop
    }
    #colnames(data) <- c("#Gen", "Eval", objsColnames, varsColnames, consColnames)
    colnames(data) <- c(objsColnames, consColnames, varsColnames, "#Gen", "#Eval")
}


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# find feasible solutions
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
extractFeasible <- function() {
    nr1 <- nrow(data)
    isFeasible <- rep(T, nr1)
    if (isFilteredByCons) {
        stopifnot(length(consColnames) == nCon)
        for (i in 1:nCon) {
            #if (max(data[, 2 + nObj + nVar + i]) >= 0) {
                #fPart <- ifelse(data[, 2 + nObj + nVar + i] >= 0, T, F)
                fPart <- ifelse(data[, nObj + i] >= 0, T, F)
                isFeasible <- isFeasible & fPart
            #} else {
            #    message("[WW] max of ", consColnames[i],
            #      " is less than zero. Skip this constraint.\n", sep = "")
            #    flush.console()
            #}
        }
    } else {
        cat("no filtering by constraints\n")
        flush.console()
    }
    return (isFeasible)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# extract non-dominated solutions from data array
# and create an array objsTmp that contains objectives of feasible solutions
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
extractNonDominatedSolutions <- function(isFeasible) {
    idxFeasible <- ifelse(isFeasible == T, 1:nrow(data), 0)
    idxFeasible <- subset(idxFeasible, idxFeasible > 0)
    #objsTmp <<- as.matrix(data[idxFeasible, 1:(2 + nObj)]) # for output
    objsTmp <<- as.matrix(data[idxFeasible, 1:(nObj + nCon)]) # for output
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# convert problem
# single objective: disable common parts objective by substituting 0
# multi objective: normalize objectives and convert problem into minimizing one
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# for indicator calculation
convertEvalObjectives <- function() {
    if (ncol(objsTmp) == 1) {
       objsTmp <<- t(objsTmp)
    }
    dataTmp <- objsTmp
    #
    eval(parse(text = conversionEvalString))
    #
    return (dataTmp)
}
#
# for prt and bst file output
convertOutObjectives <- function() {
    #
    eval(parse(text = conversionOutputString))
    #
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# merge arhive and this generation's solution
# and then rank the merged solution
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
rankUnion <- function(dataTmp, isRedefineArc) {
    # Re-define Archive arrays for write history
    if (isRedefineArc == T) {
        # initialize archive and store dataTmp as it is 
        archive <<-  as.matrix(dataTmp)
        archive <<- as.matrix(archive[complete.cases(archive[, 1]), ])
    } else {
        # achive stores only objectives
        #archive <<- as.matrix(rbind(archive[, 1:(2 + nObj)], dataTmp[, 1:(2 + nObj)]))
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
      #idd <- as.matrix(is_dominated(t(as.matrix(archive[, (2 + 1):(2 + nObj)]))))
      idd <- as.matrix(is_dominated(t(as.matrix(archive[, 1:nObj]))))
    }
    #
    rankFeasible <- ifelse(idd == F, 1, 2)
    return (rankFeasible)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# calculate indicator value
# indicatorTmp uses only this generation's solution
# indicatorArc uses whole generation's solution
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
calculateIndicator <- function(iRun2, dataTmp) {
    if(sngl == T) {
        if (nrow(dataTmp) > 0) {
            #indicatorTmp[iRun2] <<- min(dataTmp[, (2 + 1)])
            indicatorTmp[iRun2] <<- min(dataTmp[, 1])
        } else {
            indicatorTmp[iRun2]  <<- initialValue
        }
        if (nrow(archive) > 0) {
            #indicatorArc[iRun2] <<- min(archive[, (2 + 1)])
            indicatorArc[iRun2] <<- min(archive[, 1])
        }
        #else indicatorArc[iRun2] has previous value
    } else {
        if (nrow(dataTmp) > 0) {
            if (nrow(dataTmp) == 1) {
                #indicatorTmp[iRun2] <<- dominated_hypervolume((as.matrix(dataTmp[, (2 + 1):(2 + nObj)])), t(as.matrix(refPoint)))
                indicatorTmp[iRun2] <<- dominated_hypervolume((as.matrix(dataTmp[, 1:nObj])), t(as.matrix(refPoint)))
            } else {
              
              
                #indicatorTmp[iRun2] <<- dominated_hypervolume(t(as.matrix(dataTmp[, (2 + 1):(2 + nObj)])), t(as.matrix(refPoint)))
                indicatorTmp[iRun2] <<- dominated_hypervolume(t(as.matrix(dataTmp[, 1:nObj])), t(as.matrix(refPoint)))
            }
        } else {
            indicatorTmp[iRun2]  <<- initialValue
        }
        if (nrow(archive) > 0) {
            if (nrow(archive) == 1) {
                #indicatorArc[iRun2] <<- dominated_hypervolume((as.matrix(archive[, (2 + 1):(2 + nObj)])), t(as.matrix(refPoint)))
            } else {
                #indicatorArc[iRun2] <<- dominated_hypervolume(t(as.matrix(archive[, (2 + 1):(2 + nObj)])), t(as.matrix(refPoint)))
                indicatorArc[iRun2] <<- dominated_hypervolume(t(as.matrix(archive[, 1:nObj])), t(as.matrix(refPoint)))
            }
        }
        #else indicatorArc[iRun2] has previous value
    }
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# calucluate statistics of indicator for multiple runs
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
calculateStats <- function() {
    medianIndicator <<- median(indicatorArc)
    if (medianIndicator == initialValue) {
        medianIndex     <<- NULL
    } else {
        medianIndex     <<- which.min(abs(indicatorArc - median(indicatorArc)))
    }
    bestIndicator   <<- ifelse(sngl == T, min(indicatorArc), max(indicatorArc))
    if (bestIndicator == initialValue) {
        bestIndex       <<- NULL
    } else {
        bestIndex       <<- ifelse(sngl == T, which.min(indicatorArc), which.max(indicatorArc))
    }
    bestIndex       <<- ifelse(sngl == T, which.min(indicatorArc), which.max(indicatorArc))
    worstIndicator  <<- ifelse(sngl == T, max(indicatorArc), min(indicatorArc))
    worstIndex      <<- ifelse(sngl == T, which.max(indicatorArc), which.min(indicatorArc))
    meanIndicator <<- mean(indicatorArc)
    stdevIndicator  <<- sd(indicatorArc)
    #
    # here indices are irun2 base. convert to actual (filename-based) run id
    actualMedianIndex     <<- medianIndex + (firstRun - 1)
    actualBestIndex     <<- bestIndex + (firstRun - 1)
    actualWorstIndex     <<- worstIndex + (firstRun - 1)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# output statistics of indicator
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
outStats <- function() {
    #
    resultStr <- matrix(, nrow = 0, ncol = 1)
    resultStr <- rbind(resultStr, "result:\n")
    for (iRun2 in 1:nRun) {
        iRun <- iRun2 + (firstRun -1)
        resultStr <- rbind(resultStr, paste("          ", indicatorName, " = ", sprintf("%.10f",indicatorArc[iRun2]) ," in ",
                                            sprintf("%04d", iRun), " th run\n", sep = ""))
    }
    resultStr <- rbind(resultStr, "\n")
    resultStr <- rbind(resultStr, paste("   Median ", indicatorName, " = ", sprintf("%.10f",medianIndicator) ," in ",
                                        sprintf("%04d", actualMedianIndex), " th run\n", sep = ""))
    resultStr <- rbind(resultStr, paste("   Best   ", indicatorName, " = ", sprintf("%.10f",bestIndicator) , " in ",
                                        sprintf("%04d", actualBestIndex), " th run\n", sep = ""))
    resultStr <- rbind(resultStr, paste("   Worst  ", indicatorName, " = ", sprintf("%.10f",worstIndicator) , " in ",
                                        sprintf("%04d", actualWorstIndex), " th run\n", sep = ""))
    resultStr <- rbind(resultStr, paste("   Mean   ", indicatorName, " = ", sprintf("%.10f",meanIndicator) ,
                                        "             \n", sep = ""))
    resultStr <- rbind(resultStr, paste("   Stdev  ", indicatorName, " = ", sprintf("%.10f",stdevIndicator) ,
                                        "             \n", sep = ""))
    cat(resultStr)
    # write results to a file
    write.table(resultStr, resultFile, c = F, r = F, quot = F, sep = ",", eol = "")
    cat("\n")
    cat("[II] output ", resultFile, "\n")
    flush.console()
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# write info about the solutions in PF for arbit. run
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
writePrtFile <- function(actualIndex, index, verb, isWrite, filename) {
    initializeIndicator()
    #archive <<- matrix(, nrow = 0 , ncol = 2 + nObj)
    archive <<- matrix(, nrow = 0 , ncol = nObj + 2)
    data <<- matrix(, nrow = 0, ncol = nObj + nVar + nCon + 2)
    nr1prev <<- 0
    executeSel0(actualIndex, index, verb, isWrite, filename)
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# write info about convergence history for arbit. run
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
writeHistoryFile <- function(actualIndex, index, verb, isWrite, filename) {
    initializeIndicator()
    if (sngl == T) {
      dataHistory <<- matrix(, nrow = 0, ncol = 6) 
    } else {
      dataHistory <<- matrix(, nrow = 0, ncol = 5) 
    }
    #archive <<- matrix(, nrow = 0 , ncol = 2 + nObj)
    archive <<- matrix(, nrow = 0 , ncol = nObj + 2)
    data <<- matrix(, nrow = 0, ncol = nObj + nVar + nCon + 2)
    nr1prev <<- 0
    executeSel1(actualIndex, index, verb, isWrite, filename)
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #  Main   # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# # # # # # # # # # # # # # # # # # # # # # # 
# main execution
# # # # # # # # # # # # # # # # # # # # # # # 
cat("\n")
cat("# # # # # # # # # # # # # # # # # # # # # # # #\n")
#
initializeIndicator()
#
iRun <- firstRun
iRun2 <- 1
while (iRun2 <= nRun ) {
    cat("\nRun = ", iRun , "\n")
    #
    #archive <- matrix(, nrow = 0 , ncol = 2 + nObj)
    archive <- matrix(, nrow = 0 , ncol = nObj + 2)
    data <- matrix(, nrow = 0, ncol = nObj + nVar + nCon + 2)
    nr1prev <<- 0
    #
    if( paretoSel == 0 ) {
        executeSel0(iRun, iRun2, T, F, NULL)
    } else {
        executeSel1(iRun, iRun2, T, F, NULL)
    }
    #
    iRun <- iRun + 1
    iRun2 <- iRun2 + 1
    #
}

# # # # # # # # # # # # # # # # # # # # # # # 
# write results
# # # # # # # # # # # # # # # # # # # # # # # 
cat("\n\n")
cat("# # # # # # # # # # # # # # # # # # # # # # # #\n")
if (isFoundFeasible == F) {
    cat("[WW] not even one feasible solution is found during entire evoluation\n")
    cat("[WW] no file is output\n\n")
} else {
    #
    calculateStats()
    outStats()
    #
    if (isWritePrtFile == T) {
        if (!is.null(medianIndex)) {
            cat("\n")
            cat("# # # # # # # # # # # # # # # # # # # # # # # #\n")
            cat("processing prt file using " , actualMedianIndex, " th run data (Median run)\n")
            writePrtFile(actualMedianIndex, medianIndex, F, T, medianPrtFile)
        }
        if (!is.null(bestIndex)) {
            cat("\n")
            cat("# # # # # # # # # # # # # # # # # # # # # # # #\n")
            cat("processing prt file using " , actualBestIndex, " th run data (Best run)\n")
            writePrtFile(actualBestIndex, bestIndex, F, T, bestPrtFile)
        }
    }
    if (isWriteHistoryFile == T) {
        if (!is.null(medianIndex)) {
            cat("\n")
            cat("# # # # # # # # # # # # # # # # # # # # # # # #\n")
            cat("processing history file using " , actualMedianIndex, " th run data (Median run)\n")
            writeHistoryFile(actualMedianIndex, medianIndex, F, T, medianHistoryFile)
        }
        if (!is.null(bestIndex)) {
            cat("\n")
            cat("# # # # # # # # # # # # # # # # # # # # # # # #\n")
            cat("processing history file using " , actualBestIndex, " th run data (Best run)\n")
            writeHistoryFile(actualBestIndex, bestIndex, F, T, bestHistoryFile)
        }
    }
    #
}


