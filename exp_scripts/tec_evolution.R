rm(list = ls(all = TRUE))
library(smoof)
library(emoa)
library(feather)
library(compiler)
library(ggplot2)
library(ggthemes)
library(MOEADps)
library(eaf)


number.fun <- 1
repetitions <- 10

checkpoints <- (0:10) * 10000
checkpoints[1] <- 1
tec_results <-
  read_feather("~/tec/tec_results")


fun.names1 <- list()

for (i in 1:30) {
  fun.names1[[length(fun.names1) + 1]] = paste0("bibbob_", i)
}

for (i in 1:9) {
  fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
}


for (i in 1:4) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}


# for (i in 1:4) {
#   fun.names1[[length(fun.names1) + 1]] = paste0("inv_DTLZ", i)
# }



source("~/MOEADr/R/summary_moead.R")
source("~/MOEADr/R/utils.R")
source("~/MOEADr/R/loadPlotData.R")
#
strategy <-
  c("MOEA/D-PS = 50",
    "MOEA/D - Big population",
    "MOEA/D - Small population")

names <- c("moead.ps.50",
           "moead500",
           # "_moead.ps.1_50",
           "moead50")

lambda <- 50


fun_hv <- data.frame()
fun_igd <- data.frame()
results <- data.frame()

for (fun in fun.names1) {
  ref1 <- data.frame()
  print(fun)
  
  benchmark <- strsplit(fun, "[0-9]")[[1]][1]
  number <- strsplit(fun, "[A-Z]")[[1]][3]
  if (is.na(number))
    number <- -1
  if (benchmark == "DTLZ") {
    Yref <-
      as.matrix(read.table(paste0(
        "../inst/extdata/pf_data/", fun, ".2D.pf"
      )))
    colnames(Yref) <- c("f1", "f2")
    ref.point <- c(1, 1)
    # number_subproblems <-
    #   c(3, 4, 6, 8, 10, 30, 50, 100, 150, 250)
  }
  if (benchmark == "UF") {
    Yref <-
      as.matrix(read.table(paste0(
        "../inst/extdata/pf_data/", fun, ".dat"
      )))
    if (as.numeric(number) == 8 ||
        as.numeric(number) == 9 || as.numeric(number) == 10) {
      colnames(Yref) <- c("f1", "f2", "f3")
      ref.point <- c(1, 1, 1)
      # number_subproblems <-
      #   c(4, 6, 8, 10, 30, 50, 100, 150, 250)
    }
    else{
      colnames(Yref) <- c("f1", "f2")
      ref.point <- c(1, 1)
      # number_subproblems <-
      #   c(3, 4, 6, 8, 10, 30, 50, 100, 150, 250)
    }
  }
  if (benchmark == "bibbob_") {
    ref.point <- c(1, 1)
    Yref <-
      as.matrix(read.table(paste0("../inst/extdata/pf_data/UF2.dat")))
    colnames(Yref) <- c("f1", "f2")
  }
  
  
  
  
  for (j in 1:repetitions) {
    # for (lambda in number_subproblems) {
    
    moead50 <-
      loadPlotData(
        name = paste0(fun, "_moead.ps.50_", lambda, "_"),
        j = j,
        wd = "~/tec/"
      )
    
    # exit()
    moead500 <-
      loadPlotData(
        name = paste0(fun, "_moead500_", lambda, "_"),
        j = j,
        wd = "~/tec/"
      )
    
    moead.ps.50 <-
      loadPlotData(
        name = paste0(fun, "_moead50_", lambda, "_"),
        j = j,
        wd = "~/tec/"
      )
    ref1 <-
      rbind(ref1,
            moead50$Y,
            moead500$Y,
            moead.ps.50$Y)
  }
  
  
  total_hv <- data.frame()
  total_igd <- data.frame()
  
  my_hv <- data.frame()
  my_igd <- data.frame()
  stg_idx <- 1
  for (name in names) {
    print(name)
    for (my_rep in 1:repetitions) {
      moea <-
        loadPlotData(
          name = paste0(fun, "_", name, "_", lambda, "_"),
          j = my_rep,
          wd = "~/tec/"
        )
      moea$n.iter <- as.integer(moea$n.iter)
      
      
      ck_idx <- 1
      
      for (iter in 1:moea$n.iter) {
        if (iter == 1) {
          nfe <- dim(moea$X)[1]
        }
        else{
          if (name == names[2]) {
            #500
            nfe <- (iter) * 500 + 500
          }
          if (name == names[3]) {
            #50
            nfe <- (iter) * 50 + 50
          }
          if (name == names[1]) {
            #PS = 50
            nfe <- (iter) * 50 + 500
          }
        }
        
        
        
        if (nfe >= checkpoints[ck_idx] || iter == moea$n.iter) {
          if (number >= 8) {
            PF <-
              data.frame(
                cbind(
                  moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V1,
                  moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V2,
                  moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V3
                )
              )
          }
          else{
            PF <-
              data.frame(cbind(
                moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V1,
                moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V2
              ))
          }
          
          if (ck_idx >= length(checkpoints)) {
            my.iter <- 100000
          }
          else{
            my.iter <- checkpoints[ck_idx]
          }
          
          
          # calc HV
          colnames(PF) <- colnames(ref1)
          
          my_igd <-
            rbind(my_igd,
                  cbind(
                    igd = igd(PF, Yref),
                    iter = my.iter,
                    fun = fun,
                    Strategy = paste0(strategy[stg_idx], "_", lambda)
                  ))
          
          PF <- scaling_Y(PF, ref1)
          hv <- dominated_hypervolume(t(PF), ref = ref.point)
          
          
          my_hv <-
            rbind(my_hv,
                  cbind(
                    hv = hv,
                    iter = my.iter,
                    fun = fun,
                    Strategy = paste0(strategy[stg_idx], "_", lambda)
                  ))
          ck_idx <- ck_idx + 1
          if (checkpoints[ck_idx] == checkpoints[length(checkpoints)]) break
        }
      }
      
      nfe <- 0
      total_hv <- rbind(total_hv, my_hv)
      total_igd <- rbind(total_igd, my_igd)
    }
    
    
    stg_idx <- stg_idx + 1
    
  }
  
  fun_hv <-
    rbind(fun_hv, total_hv)
  
  fun_igd <-
    rbind(fun_igd, total_igd)
  
}
fun_hv$hv <- as.numeric(as.character(fun_hv$hv))
fun_igd$igd <- as.numeric(as.character(fun_igd$igd))


write_feather(fun_hv, "fun_hv")
write_feather(fun_igd, "fun_igd")
