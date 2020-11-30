options(kableExtra.latex.load_packages = T)
library(kableExtra)
library(feather)

tec_results <-  read_feather("~/tec/tec_results")


# HV

tec_results$hv <- round(tec_results$hv, 2)
# tec_results <- tec_results[c(tec_results$fun == c("bibbob_55", "bibbob_54", "bibbob_53", "bibbob_50", "bibbob_47", "bibbob_46", "bibbob_41", "bibbob_36", "bibbob_35", "bibbob_28", "bibbob_21", "bibbob_20", "bibbob_11", "bibbob_2", "bibbob_1", "UF1", "UF2", "UF3", "UF4", "UF5", "UF6", "UF7", "UF8", "UF9" , "UF10", "DTLZ1", "DTLZ2", "DTLZ3", "DTLZ4", "inv_DTLZ1", "inv_DTLZ2", "inv_DTLZ3", "inv_DTLZ4")),]

data.k <- data.frame()
for (fun in unique(tec_results$fun)){
  data.hv2 <- tec_results[tec_results$fun == fun,]
  data.hv.median <- aggregate(data.hv2$hv, mean, by = list(data.hv2$name, data.hv2$fun))
  data.hv.median <- as.data.frame(t(data.hv.median[,c(1,3)]))
  # data.hv.median$FUN <- fun
  data.k <- rbind(data.k, data.hv.median[2,])
  
}



rownames(data.k) <- unique(tec_results$fun)
data.k <- data.k[,c(3,2,1)]
colnames(data.k) <- c("MOEA/D-PS = 50", "MOEA/D - Big pop.","MOEA/D - small pop.")
# data.k <- data.k[,c(3,2,1)]
kbl(data.k, booktabs = T, format = "latex")

# NNON-DOM
options(kableExtra.latex.load_packages = T)
library(kableExtra)
library(feather)

tec_results <-  read_feather("~/tec/tec_results")

tec_results$nndom <- round(tec_results$nndom, 2)
# tec_results <- tec_results[c(tec_results$fun == c("bibbob_55", "bibbob_54", "bibbob_53", "bibbob_50", "bibbob_47", "bibbob_46", "bibbob_41", "bibbob_36", "bibbob_35", "bibbob_28", "bibbob_21", "bibbob_20", "bibbob_11", "bibbob_2", "bibbob_1", "UF1", "UF2", "UF3", "UF4", "UF5", "UF6", "UF7", "UF8", "UF9" , "UF10", "DTLZ1", "DTLZ2", "DTLZ3", "DTLZ4", "inv_DTLZ1", "inv_DTLZ2", "inv_DTLZ3", "inv_DTLZ4")),]

data.k <- data.frame()
for (fun in unique(tec_results$fun)){
  data.hv2 <- tec_results[tec_results$fun == fun,]
  data.hv.median <- aggregate(data.hv2$nndom, mean, by = list(data.hv2$name, data.hv2$fun))
  data.hv.median$x <- data.hv.median$x/500
  data.hv.median[2,] <- data.hv.median[2,]
  data.hv.median <- as.data.frame(t(data.hv.median[,c(1,3)]))
  # data.hv.median$FUN <- fun
  data.k <- rbind(data.k, data.hv.median[2,])
}


rownames(data.k) <- unique(tec_results$fun)
data.k <- data.k[,c(3,2,1)]
colnames(data.k) <- c("MOEA/D-PS = 50", "MOEA/D - Big pop.","MOEA/D - small pop.")
# data.k <- data.k[,c(3,2,1)]
kbl(data.k, booktabs = T, format = "latex")
