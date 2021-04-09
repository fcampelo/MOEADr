rm(list = ls(all = TRUE))
library(smoof)
library(emoa)
library(feather)
library(compiler)
library(ggplot2)
library(ggthemes)
library(MOEADps)
library(eaf)

source("~/MOEADr/R/utils.R")

number.fun <- 1
repetitions <- 10

checkpoints <- (0:20) * 5000
checkpoints[1] <- 500


fun.names1 <- list()

for (i in 1:55) {
  fun.names1[[length(fun.names1) + 1]] = paste0(i)
}
#
for (i in 1:10) {
  fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
}


for (i in 1:4) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}
#
#
for (i in 1:4) {
  fun.names1[[length(fun.names1) + 1]] = paste0("inv_DTLZ", i)
}

PF <- data.frame()

source("~/MOEADr/R/summary_moeadps.R")
source("~/MOEADr/R/utils.R")
# source("~/MOEADr/R/loadPlotData.R")
#
strategy <-
  c("MOEA/D-PS",
    "Big pop.",
    "Small pop.")

names <- c("moead.ps.50",
           "moead500",
           "moead50")

lambda <- 50


fun_hv <- data.frame()
results <- data.frame()

for (fun in fun.names1) {
  ref1 <- data.frame()
  print(fun)
  ref.point <- c(1, 1)
  divs <- 50
  benchmark <- strsplit(fun, "[0-9]")[[1]][1]
  number <- as.numeric(strsplit(fun, "[A-Z]")[[1]][3])
  if (is.na(number))
    number <- -1
  if (benchmark == "UF") {
    if (number == 8 ||
        number == 9 || number == 10) {
      ref.point <- c(1, 1, 1)
      divs <- 53
    }
  }
  
  
  for (j in 1:repetitions) {
    moead50 <-
      loadPlotData(
        name = paste0(fun, "_moead50_", lambda, "_"),
        j = j,
        wd = "~/tec/"
      )
    moead50$n.iter <- as.integer(moead50$n.iter)
    tmp1 <- data.frame()
    # for (i in moead50$n.iter:moead50$n.iter) {
    #   if (benchmark == "UF" && number >= 8) {
    #     tmp1 <- rbind(
    #       tmp1,
    #       cbind(
    #         moead50$plot.paretofront[moead50$plot.paretofront$stage == i, ]$V1,
    #         moead50$plot.paretofront[moead50$plot.paretofront$stage == i, ]$V2,
    #         moead50$plot.paretofront[moead50$plot.paretofront$stage == i, ]$V3
    #       )
    #     )
    #   }
    #   else{
    #     tmp1 <- rbind(tmp1,
    #                   cbind(
    #                     moead50$plot.paretofront[moead50$plot.paretofront$stage == i, ]$V1,
    #                     moead50$plot.paretofront[moead50$plot.paretofront$stage == i, ]$V2
    #                   ))
    #   }
    # }
    
    if (benchmark == "UF" && number >= 8) {
      tmp1 <- rbind(
        tmp1,
        cbind(
          moead50$plot.paretofront[moead50$plot.paretofront$stage == moead50$n.iter, ]$V1,
          moead50$plot.paretofront[moead50$plot.paretofront$stage == moead50$n.iter, ]$V2,
          moead50$plot.paretofront[moead50$plot.paretofront$stage == moead50$n.iter, ]$V3
        )
      )
    }
    else{
      tmp1 <- rbind(tmp1,
                    cbind(
                      moead50$plot.paretofront[moead50$plot.paretofront$stage == moead50$n.iter, ]$V1,
                      moead50$plot.paretofront[moead50$plot.paretofront$stage == moead50$n.iter, ]$V2
                    ))
    }
    
    moead500 <-
      loadPlotData(
        name = paste0(fun, "_moead500_", lambda, "_"),
        j = j,
        wd = "~/tec/"
      )
    moead500$n.iter <- as.integer(moead500$n.iter)
    tmp2 <- data.frame()
    # for (i in 1:moead500$n.iter) {
    #   if (benchmark == "UF" && number >= 8) {
    #     tmp2 <- rbind(
    #       tmp2,
    #       cbind(
    #         moead500$plot.paretofront[moead500$plot.paretofront$stage == i, ]$V1,
    #         moead500$plot.paretofront[moead500$plot.paretofront$stage == i, ]$V2,
    #         moead500$plot.paretofront[moead500$plot.paretofront$stage == i, ]$V3
    #       )
    #     )
    #   }
    #   else{
    #     tmp2 <- rbind(
    #       tmp2,
    #       cbind(
    #         moead500$plot.paretofront[moead500$plot.paretofront$stage == i, ]$V1,
    #         moead500$plot.paretofront[moead500$plot.paretofront$stage == i, ]$V2
    #       )
    #     )
    #   }
    # }
    if (benchmark == "UF" && number >= 8) {
      tmp2 <- rbind(
        tmp2,
        cbind(
          moead500$plot.paretofront[moead500$plot.paretofront$stage == moead500$n.iter, ]$V1,
          moead500$plot.paretofront[moead500$plot.paretofront$stage == moead500$n.iter, ]$V2,
          moead500$plot.paretofront[moead500$plot.paretofront$stage == moead500$n.iter, ]$V3
        )
      )
    }
    else{
      tmp2 <- rbind(
        tmp2,
        cbind(
          moead500$plot.paretofront[moead500$plot.paretofront$stage == moead500$n.iter, ]$V1,
          moead500$plot.paretofront[moead500$plot.paretofront$stage == moead500$n.iter, ]$V2
        )
      )
    }
    
    moead.ps.50 <-
      loadPlotData(
        name = paste0(fun, "_moead.ps.50_", lambda, "_"),
        j = j,
        wd = "~/tec/"
      )
    moead.ps.50$n.iter <- as.integer(moead.ps.50$n.iter)
    tmp3 <- data.frame()
    # for (i in 1:moead50$n.iter) {
    #   if (benchmark == "UF" && number >= 8) {
    #     tmp3 <- rbind(
    #       tmp3,
    #       cbind(
    #         moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == i, ]$V1,
    #         moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == i, ]$V2,
    #         moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == i, ]$V3
    #       )
    #     )
    #   }
    #   else{
    #     tmp3 <- rbind(
    #       tmp3,
    #       cbind(
    #         moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == i, ]$V1,
    #         moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == i, ]$V2
    #       )
    #     )
    #   }
    # }
    if (benchmark == "UF" && number >= 8) {
      tmp3 <- rbind(
        tmp3,
        cbind(
          moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == moead.ps.50$n.iter, ]$V1,
          moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == moead.ps.50$n.iter, ]$V2,
          moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == moead.ps.50$n.iter, ]$V3
        )
      )
    }
    else{
      tmp3 <- rbind(
        tmp3,
        cbind(
          moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == moead.ps.50$n.iter, ]$V1,
          moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == moead.ps.50$n.iter, ]$V2
        )
      )
    }
    
    ref1 <-
      rbind(ref1,
            tmp1,
            tmp2,
            tmp3)
  }
  
  
  #############################################################################################################################
  #############################################################################################################################
  #############################################################################################################################
  
  
  
  
  for (my_rep in 1:repetitions) {
    moead.ps.50 <-
      loadPlotData(
        name = paste0(fun, "_moead.ps.50_", lambda, "_"),
        j = my_rep,
        wd = "~/tec/"
      )
    iter <- as.integer(moead.ps.50$n.iter)
    
    if (benchmark == "UF" && number >= 8) {
      moead.ps.50$Y <- cbind(
        moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == iter, ]$V1,
        moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == iter, ]$V2,
        moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == iter, ]$V3
      )
    }
    else{
      moead.ps.50$Y <-
        cbind(moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == iter, ]$V1,
              moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == iter, ]$V2)
    }
    
    
    moead500 <-
      loadPlotData(
        name = paste0(fun, "_moead500_", lambda, "_"),
        j = my_rep,
        wd = "~/tec/"
      )
    iter <- as.integer(moead500$n.iter)
    
    if (benchmark == "UF" && number >= 8) {
      moead500$Y <- cbind(
        moead500$plot.paretofront[moead500$plot.paretofront$stage == iter, ]$V1,
        moead500$plot.paretofront[moead500$plot.paretofront$stage == iter, ]$V2,
        moead500$plot.paretofront[moead500$plot.paretofront$stage == iter, ]$V3
      )
    }
    else{
      moead500$Y <-
        cbind(moead500$plot.paretofront[moead500$plot.paretofront$stage == iter, ]$V1,
              moead500$plot.paretofront[moead500$plot.paretofront$stage == iter, ]$V2)
    }
    
    moead50 <-
      loadPlotData(
        name = paste0(fun, "_moead50_", lambda, "_"),
        j = my_rep,
        wd = "~/tec/"
      )
    n.iter <- as.integer(moead.ps.50$n.iter)
    tmp <- data.frame()
    if (benchmark == "UF" && number >= 8) {
      
      for (x in 0:9) {
        tmp <- rbind(tmp,cbind(
          moead50$plot.paretofront[moead50$plot.paretofront$stage == iter - x, ]$V1,
          moead50$plot.paretofront[moead50$plot.paretofront$stage == iter - x, ]$V2,
          moead50$plot.paretofront[moead50$plot.paretofront$stage == iter - x, ]$V3
        ))
        
      }
    }
    else{
      for (x in 0:9) {
        tmp <-
          rbind(tmp,cbind(moead50$plot.paretofront[moead50$plot.paretofront$stage == iter - x, ]$V1,
                moead50$plot.paretofront[moead50$plot.paretofront$stage == iter - x, ]$V2))
      }
    }
    moead50$Y <- tmp
    class(moead50) <- "moead"
    class(moead500) <- "moead"
    class(moead.ps.50) <- "moead"
    
    # print("names here")
    colnames(moead50$Y) <- colnames(ref1)
    colnames(moead500$Y) <- colnames(ref1)
    colnames(moead.ps.50$Y) <- colnames(ref1)
    
    PF <-
      rbind(PF, data.frame(
        scaling_Y(moead50$Y, ref1),
        set = my_rep,
        name = "moead50",
        fun = fun
      ))
    PF <-
      rbind(PF, data.frame(
        scaling_Y(moead500$Y, ref1),
        set = my_rep,
        name = "moead500",
        fun = fun
      ))
    PF <-
      rbind(PF, data.frame(
        scaling_Y(moead.ps.50$Y, ref1),
        set = my_rep,
        name = "moead.ps.50",
        fun = fun
      ))
  }
}


for (fun in unique(PF$fun)){
  per.fun <- PF[PF$fun == fun,]
  A1 <- per.fun[per.fun$name == "moead50",]
  A2 <- per.fun[per.fun$name == "moead500",]
  A3 <- per.fun[per.fun$name == "moead.ps.50",]
  
  
  Small <- A1[, c(1:3)]
  Big <- A2[, c(1:3)]
  PS <- A3[, c(1:3)]
  
  
  setEPS()
  postscript(file = paste0("~/tec/eaf/eaf_", fun,"_small_vs_ps.eps"), width = 30.48*2.2, height = 30.48*1.2)
  eafdiffplot(
    data.left = PS,
    data.right = Small,
    full.eaf = F,
    legend.pos = "bottomleft",
    col = c("white", "orange", "red"),
    title.left = "MOEA/D-PS",
    title.right = "Small population",
    intervals = 5,
    cex = 7,
    cex.lab = 7
  )
  dev.off()
  
  setEPS()
  postscript(file = paste0("~/tec/eaf/eaf_", fun,"_big_vs_ps.eps"), width = 30.48*2.2, height = 30.48*1.2)
  # png(file = paste0("~/tec/eaf/eaf_", fun,"_big_vs_ps.png"),res = 300,
  #     width = 30.48*2,
  #     height = 30.48, units = "cm")
  eafdiffplot(
    data.left = PS,
    data.right = Big,
    full.eaf = F,
    legend.pos = "bottomleft",
    col = c("white", "orange", "red"),
    title.left = "MOEA/D-PS",
    title.right = "Big population",
    intervals = 5,
    cex=7,
    cex.lab = 7
  )
  dev.off()
  # exit( ) 
  
  
}
