rm(list = ls(all = TRUE))
setwd("~/myMOEADr/dataMOEADr/")
library(feather)
library(ggplot2) # used
library(tidyverse) # used
library(hrbrthemes) # used
library(plyr) # used
library(viridis) # used

results_2 <- read_feather("results_uf2")
results_3 <- read_feather("results_uf3")
results_DTLZ <- read_feather("results_DTLZ")
results_fixed <- read_feather("~/myMOEADr/dataMOEADr/results_fixed")


results <- rbind(results_2, results_3, results_DTLZ)

unique(results$fun)
unique(results$name)

# pairwise.wilcox.test(results_DTLZ$hv, results_DTLZ$name, p.adjust.method = "hommel")
# pairwise.wilcox.test(results_UF$hv, results_DTLZ$name, p.adjust.method = "hommel")
pairwise.wilcox.test(results_fixed$hv, results_fixed$name, p.adjust.method = "hommel")
pairwise.wilcox.test(results_fixed$igd, results_fixed$name, p.adjust.method = "hommel")

boxplot(results_fixed$hv ~ results_fixed$name)

for (fun in unique(results_fixed$fun)){
  print(fun)
  temp <- results_fixed[results_fixed$fun == fun,]
  # print(temp)
  for (name in unique(temp$name)){
    print(name)
    temp2 <- temp[temp$name == name,]
    # print(temp2)
    cat("hv", round(median(temp2$hv),3), "(",round(sd(temp2$hv),3),")", "\n")
    cat("igd", round(median(temp2$igd),3), "(",round(sd(temp2$igd),3),")", "\n")
    print(median(temp2$nndom)/350)

    readline("next:")
    # break
  }
  # break
}

levels(results_fixed$name) <-
  c("10%",
    "20%",
    "40%",
    "60%",
    "80%",
    "None",
    "DS",
    "R.I.")

for (fun in unique(results_fixed$fun)){
  print(fun)
  temp <- results_fixed[results_fixed$fun == fun,]
  # temp <- temp[temp$name != "DS",]
  # temp <- temp[temp$name != "R.I.",]
  # temp$name <- factor(levels(temp$name)[-c(7,8)])
  boxplot(temp$hv ~ temp$name, main = fun)
}

for (fun in unique(results_fixed$fun)){
  print(fun)
  temp <- results_fixed[results_fixed$fun == fun,]
  temp <- temp[temp$name != "DS",]
  temp <- temp[temp$name != "R.I.",]
  temp$name <- factor(levels(temp$name)[-c(7,8)])
  # print(temp)
  for (name in unique(temp$name)){
    cat(name)
    temp2 <- temp[temp$name == name,]
    print("nondom")
    print(round(median(temp2$nndom)/350,2)*100)
    print(round(sd(temp2$nndom)/350,2)*100)
    print("hv")
    print(round(median(temp2$hv),3))
    print(round(sd(temp2$hv),3))
    print("igd")
    print(round(median(temp2$igd),3))
    print(round(sd(temp2$igd),3))
    readline(prompt="Next: ")
  }
  
}
