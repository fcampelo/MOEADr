rm(list = ls(all = TRUE))

library(feather)
library(ggplot2)
library(ggthemes)
library(eaf)

number.fun <- 1
repetitions <- 10
lambda <- 50
checkpoints <- (0:40) * 750
checkpoints[1] <- 1
jpnsec_results <- read_feather("~/jsec_2020_50/inv_jpnsec_results")

fun.names1 <- list()

# for (i in 1:7) {
#   fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
# }
for (i in 1:7) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}


# number_subproblems <-
#  c(3, 4, 5, 6, 7, 8, 9, 10, 30, 50, 100, 150, 250)
source("~/MOEADr/R/summary_moeadps.R")
source("~/MOEADr/R/utils.R")
source("~/MOEADr/R/loadPlotData.R")

strategy <-
  c("MOEA/D with PS = 50",
    "Big Population",
    "Small Population")

names <- c("moead.random",
           "moead500",
           "moead.3")

PF <- data.frame()

for (fun in fun.names1) {
  print(fun)
  ref1 <- data.frame()
  for (j in 1:repetitions) {
    moead.random <-
      loadPlotData(
        name = paste0("inv_",fun, "_moead.random_", lambda, "_"),
        wd = "~/jsec_2020_50/",
        j = j
      )
    moead3 <-
      loadPlotData(
        name = paste0("inv_",fun, "_moead.3_", lambda, "_"),
        wd = "~/jsec_2020_50/",
        j = j
      )
    
    moead500 <-
      loadPlotData(
        name = paste0("inv_",fun, "_moead500_", lambda, "_"),
        wd = "~/jsec_2020_50/",
        j = j
      )
    ref1 <-
      rbind(ref1,
            moead.random$Y,
            moead3$Y,
            moead500$Y)
  }
  
  for (name in names) {
    print(name)
    for (my_rep in 1:repetitions) {
      moea <-
        loadPlotData(
          name = paste0("inv_", fun, "_", name, "_", lambda, "_"),
          j = my_rep,
          wd = "~/jsec_2020_50/"
        )
      moea$n.iter <- as.integer(moea$n.iter)
      # sampled <-
      #   sort(seq(to = moea$n.iter, by = ceiling(moea$n.iter / 10)))
      # for (iter in moea$n.iter) {
      temp <- cbind(moea$plot.paretofront[moea$plot.paretofront$stage == moea$n.iter, ]$V1,
                    moea$plot.paretofront[moea$plot.paretofront$stage == moea$n.iter, ]$V2)
      colnames(temp) <- colnames(ref1)
      PF <-
        rbind(PF, data.frame(
          scaling_Y(temp, ref1),
          set = my_rep,
          name = name,
          fun = fun
        ))
      
      # }
      
    }
    
    
  }
}

for (fun in unique(PF$fun)){
  per.fun <- PF[PF$fun == fun,]
  A1 <- per.fun[per.fun$name == "moead.3",]
  A2 <- per.fun[per.fun$name == "moead500",]
  A3 <- per.fun[per.fun$name == "moead.random",]
  
  Small <- A1[, c(1:3)]
  Big <- A2[, c(1:3)]
  PS <- A3[, c(1:3)]
  
  
  setEPS()
  postscript(file = paste0("~/jsec_2020_50/eaf/eaf_", fun,"_small_vs_ps.eps"), width = 30.48*2, height = 30.48*1.2)
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
  postscript(file = paste0("~/jsec_2020_50/eaf/eaf_", fun,"_big_vs_ps.eps"), width = 30.48*2, height = 30.48*1.2)
  # png(file = paste0("~/jsec_2020_50/eaf/eaf_", fun,"_big_vs_ps.png"),res = 300,
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
# res <- vorobT(PS, reference = c(2, 200))
# eafplot(PS[,1:2], sets = PS[,3], percentiles = c(0, 20, 40, 60, 80, 100), type = "area", 
#         legend.pos = "bottomleft", extra.points = res$VE, extra.col = "cyan",
#         extra.legend = "VE", extra.lty = "solid", extra.pch = NA, extra.lwd = 2,
#         main = substitute(paste("Empirical attainment function, ",beta,"* = ", a, "%"),
#                           list(a = formatC(res$threshold, digits = 2, format = "f"))))
# 
# res <- vorobT(Small, reference = c(2, 200))
# eafplot(Small[,1:2], sets = Small[,3], percentiles = c(0, 20, 40, 60, 80, 100), type = "area", 
#         legend.pos = "bottomleft", extra.points = res$VE, extra.col = "cyan",
#         extra.legend = "VE", extra.lty = "solid", extra.pch = NA, extra.lwd = 2,
#         main = substitute(paste("Empirical attainment function, ",beta,"* = ", a, "%"),
#                           list(a = formatC(res$threshold, digits = 2, format = "f"))))
# 
# res <- vorobT(Big, reference = c(2, 200))
# eafplot(Big[,1:2], sets = Big[,3], percentiles = c(0, 20, 40, 60, 80, 100), type = "area", 
#         legend.pos = "bottomleft", extra.points = res$VE, extra.col = "cyan",
#         extra.legend = "VE", extra.lty = "solid", extra.pch = NA, extra.lwd = 2,
#         main = substitute(paste("Empirical attainment function, ",beta,"* = ", a, "%"),
#                           list(a = formatC(res$threshold, digits = 2, format = "f"))))
