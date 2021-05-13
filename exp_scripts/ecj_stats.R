rm(list = ls(all = TRUE))
library(smoof)
library(emoa)
library(feather)
library(compiler)
library(ggplot2)
library(ggthemes)
library(MOEADps)
library(eaf)
options(kableExtra.latex.load_packages = T)
library(kableExtra)
options(scipen=0)

source("~/MOEADr/R/utils.R")


stats0 <- read_feather("~/tec/ecj_stats")


p.values <- data.frame()


for (iter in unique(stats0$iter)) {
  stats1 <- stats0[stats0$iter == iter, ]
  print(iter)
  # stats1 <- stats1[stats1$Strategy != "Small pop.",]
  stats2 <-
    aggregate(stats1$hv, median, by = list(stats1$Strategy, stats1$fun))
  colnames(stats2) <- c("Strategy", "fun", "hv")
  
  tmp <- pairwise.wilcox.test(
    stats2$hv,
    stats2$Strategy,
    paired = TRUE,
    p.adj = "hommel",
    conf.int = TRUE
  )$p.value
  print(tmp)
  # tmp <- c(tmp)[-3]
  
  # p.values <- rbind(p.values, c(tmp, as.numeric(iter)))z
}

# tmp2 <- p.values
# p.values <- p.values[-4]
# p.values <- as.data.frame(matrix(t(apply(p.values, FUN = formatC, MARGIN = 1, format = "e", digits = 2)), ncol = 3))
# 
# p.values[,4] <- tmp2[,4]
# p.values <- p.values[c(4,1,2,3)]
# kbl(p.values, booktabs = T, format = "latex")



# for (iter in unique(stats0$iter)) {
#   stats1 <- stats0[stats0$iter == iter, ]
#   stats2 <-
#     aggregate(stats1$hv, median, by = list(stats1$Strategy, stats1$fun))
#   colnames(stats2) <- c("Strategy", "fun", "hv")
#   tmp <- pairwise.wilcox.test(
#     stats2$hv,
#     stats2$Strategy,
#     paired = TRUE,
#     p.adj = "hommel",
#     conf.int = TRUE
#   )$p.value
#   
#   tmp <- c(tmp)[-3]
#   
#   p.values <- rbind(p.values, c(tmp, as.numeric(iter)))
# }
# 
# tmp2 <- p.values
# p.values <- p.values[-4]
# p.values <- as.data.frame(matrix(t(apply(p.values, FUN = formatC, MARGIN = 1, format = "e", digits = 2)), ncol = 3))
# 
# p.values[,4] <- tmp2[,4]
# p.values <- p.values[c(4,1,2,3)]
# kbl(p.values, booktabs = T, format = "latex")
# 

