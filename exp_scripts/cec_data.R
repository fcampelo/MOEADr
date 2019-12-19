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
results_fixed <- read_feather("results_fixed")


results <- rbind(results_2, results_3, results_DTLZ)

unique(results$fun)
unique(results$name)

# pairwise.wilcox.test(results_DTLZ$hv, results_DTLZ$name, p.adjust.method = "hommel")
# pairwise.wilcox.test(results_UF$hv, results_DTLZ$name, p.adjust.method = "hommel")
pairwise.wilcox.test(results$hv, results$name, p.adjust.method = "hommel")
pairwise.wilcox.test(results$igd, results$name, p.adjust.method = "hommel")

aggregate(results$nndom, function(x){x/350}, by = list(results$name))

for (fun in unique(results$fun)){
  print(fun)
  temp <- results[results$fun == fun,]
  # print(temp)
  for (name in unique(temp$name)){
    cat(name)
    temp2 <- temp[temp$name == name,]
    # print(temp2)
    print(median(temp2$nndom)/350)
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


for (fun in unique(results_fixed$fun)){
  print(fun)
  temp <- results_fixed[results_fixed$fun == fun,]
  for (name in unique(temp$name)){
    cat(name)
    temp2 <- temp[temp$name == name,]
    cat(" IGD median: ",which(temp2$igd == median(temp2$igd)))
    cat(" HV median: ",which(temp2$hv == median(temp2$hv)))
    cat("\n")
     
    moea <-
      loadPlotData(name = paste0(fun, "moead.random_fixed.0.05"),
                   j = j)
    

  moea.final.iter <-
      moead.de$plot.paretofront[moea$plot.paretofront$stage == max(moea$plot.paretofront$stage),]
  moea.final.nndom <- moea.final.iter[moea.final.iter$`non-dominated` == 1,]
  moea.final.nndom <- as.data.frame(cbind(moea.final.nndom$f1, moea.final.nndom$f2))
    names(moea.final.nndom) <- c("f1", "f2")
    moea.final.nndom <- scaling_Y(Y = moea.final.nndom, X = ref1)
  }
    
  plots.data <-
      rbind(
        plots.data,
        moea.final.nndom
      )
    
    
    # v <-
    #   ggplot(plots.data, aes(f1, f2)) + geom_line(aes(color = Strategy)) + geom_point(aes(shape = Strategy, color = Strategy), size=2) + theme(
    #     legend.text = element_text(size =
    #                                  18),
    #     axis.text = element_text(size =
    #                                14 *
    #                                2),
    #     axis.title =
    #       element_text(size = 14 *
    #                      2, face = "bold")
    #   ) + theme(plot.title = element_text(
    #     color = "blue",
    #     size = 24 *
    #       3,
    #     face = "bold"
    #   ))
    # v + scale_colour_brewer(palette = "Dark2")
    # filename = paste0("~/Desktop/",name, fun,"igd_pareto_fronts.eps")
    # ggsave(filename = filename, dpi = 600)
  }




rm(list = ls(all = TRUE))
setwd("~/myMOEADr/dataMOEADr/")
library(feather)
library(ggplot2) # used
library(tidyverse) # used
library(hrbrthemes) # used
library(plyr) # used
library(viridis) # used

# lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)
fun.names1 <- list()
source("visualization_tools.R")
number.fun <- 1
repetitions <- 21
j <- 1

results_dtlz7 <- read_feather("results_fixed_DTLZ7")
results_UF10 <- read_feather("results_fixed_UF10")

unique(results_UF10$name)

# results_UF10 <-
#   subset(results_UF10, subset = results_UF10$name != list("moead.random_fixed.0.05"))
# results_UF10$name <- factor(results_UF10$name)
# results_UF10 <-
#   subset(results_UF10, subset = results_UF10$name != list("moead.random_fixed.0.15"))
# results_UF10$name <- factor(results_UF10$name)



results_dtlz7 <- read_feather("results_fixed_DTLZ7")
unique(results_dtlz7$name)
# results_dtlz7 <-
#   subset(results_dtlz7,
#          subset = results_dtlz7$name != list("moead.random_fixed.0.05"))
# 
# 
# results_dtlz7 <-
#   subset(results_dtlz7,
#          subset = results_dtlz7$name != list("moead.random_fixed.0.15"))
# results_dtlz7$name <- factor(results_dtlz7$name)
# unique(results_dtlz7$name)


x <- results_UF10$name
results_UF10$name <-
  revalue(
    x,
    c(
      "moead.random_fixed.1.0" = "Threshold-1.0",
      "moead.random_fixed.0.8" = "Threshold-0.8",
      "moead.random_fixed.0.6" = "Threshold-0.6",
      "moead.random_fixed.0.4" = "Threshold-0.4",
      "moead.random_fixed.0.2" = "Threshold-0.2",
      "moead.random_fixed.0.15" = "Threshold-0.15",
      "moead.random_fixed.0.1" = "Threshold-0.1",
      "moead.random_fixed.0.05" = "Threshold-0.05"
    )
  )

x <- results_dtlz7$name
results_dtlz7$name <-
  revalue(
    x,
    c(
      "moead.random_fixed.1.0" = "Threshold-1.0",
      "moead.random_fixed.0.8" = "Threshold-0.8",
      "moead.random_fixed.0.6" = "Threshold-0.6",
      "moead.random_fixed.0.4" = "Threshold-0.4",
      "moead.random_fixed.0.2" = "Threshold-0.2",
      "moead.random_fixed.0.15" = "Fix-0.15",
      "moead.random_fixed.0.1" = "Threshold-0.1",
      "moead.random_fixed.0.05" = "Fix-0.05"
    )
  )

results_UF10 %>%
  ggplot(aes(x = name, y = igd, fill = name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(legend.position = "none",
        plot.title = element_text(size = 11)) +
  ggtitle("UF10") +
  xlab("") + ylab("")

results_dtlz7 %>%
  ggplot(aes(x = name, y = igd, fill = name)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(legend.position = "none",
        plot.title = element_text(size = 11)) + xlab("") + ylab("") +
  ggtitle("DTLZ'") +
  xlab("Threshold Priority Function")

print(aggregate(
  results_UF10$igd,
  median,
  by = list(results_UF10$name, results_UF10$fun)
))
print(aggregate(
  results_UF10$igd,
  sd,
  by = list(results_UF10$name, results_UF10$fun)
))

a <-
  (aggregate(
    results_UF10$nndom / 351,
    median,
    by = list(results_UF10$name, results_UF10$fun)
  ))
round(a$x, 3)
b <-
  (aggregate(
    results_UF10$nndom / 351,
    sd,
    by = list(results_UF10$name, results_UF10$fun)
  ))
round(b$x, 3)

a <-
  (aggregate(
    results_dtlz7$igd,
    median,
    by = list(results_dtlz7$name, results_dtlz7$fun)
  ))
round(a$x, 3)
b <-
  (aggregate(
    results_dtlz7$igd,
    sd,
    by = list(results_dtlz7$name, results_dtlz7$fun)
  ))
round(b$x, 3)
a <-
  (aggregate(
    results_dtlz7$nndom / 351,
    median,
    by = list(results_dtlz7$name, results_dtlz7$fun)
  ))
round(a$x, 3)
b <-
  (aggregate(
    results_dtlz7$nndom / 351,
    sd,
    by = list(results_dtlz7$name, results_dtlz7$fun)
  ))
round(b$x, 3)



results <- rbind(results_UF10, results_dtlz7)

pairwise.wilcox.test(results$igd, results$name, p.adjust.method = "hommel")

pairwise.wilcox.test(results_UF10$hv, results_UF10$name, p.adjust.method = "hommel")
pairwise.wilcox.test(results_UF10$igd, results_UF10$name, p.adjust.method = "hommel")

pairwise.wilcox.test(results_dtlz7$hv, results_dtlz7$name, p.adjust.method = "hommel")
pairwise.wilcox.test(results_dtlz7$igd, results_dtlz7$name, p.adjust.method = "hommel")


fun <- "DTLZ7"

Yref <-
  as.matrix(read.table(paste0(
    "~/MOEADr/inst/extdata/pf_data/", fun, ".2d.pf"
  )))
if (fun == "DTLZ7")
  colnames(Yref) <- c("f1", "f2")
# if (fun == "UF10") colnames(Yref) <- c("f1", "f2", "f3")

moead.random_fixed.0.05 <-
  loadPlotData(name = paste0(fun, "moead.random_fixed.0.05"),
               j = j)
moead.random_fixed.0.1 <-
  loadPlotData(name = paste0(fun, "moead.random_fixed.0.1"),
               j = j)
moead.random_fixed.0.15 <-
  loadPlotData(name = paste0(fun, "moead.random_fixed.0.15"),
               j = j)
moead.random_fixed.0.2 <-
  loadPlotData(name = paste0(fun, "moead.random_fixed.0.2"),
               j = j)

moead.random_fixed.0.4 <-
  loadPlotData(name = paste0(fun, "moead.random_fixed.0.4"),
               j = j)

moead.random_fixed.0.6 <-
  loadPlotData(name = paste0(fun, "moead.random_fixed.0.6"),
               j = j)

moead.random_fixed.0.8 <-
  loadPlotData(name = paste0(fun, "moead.random_fixed.0.8"),
               j = j)

moead.random_fixed.1.0 <-
  loadPlotData(name = paste0(fun, "moead.random_fixed.1.0"),
               j = j)

moea <- moead.random_fixed.0.05
n.iter <- as.numeric(moea$n.iter)
my_igd <- data.frame()
n.iter <- n.iter - 1
for (i in 0:n.iter) {
  idxs <- ((350 * i) + 1):(350 * (i + 1))
  PF <-
    cbind(moea$plot.paretofront$f1[idxs], moea$plot.paretofront$f2[idxs])
  # colnames(PF) <- c("V1", "V2")
  igd <- calcIGD(Y = PF, Yref = Yref)
  my_igd <- rbind(igd, my_igd)
}
pdf(file = "~/igd_0.05.png")
plot(
  x = nrow(my_igd):1,
  y = my_igd$X,
  xlab = "iteration",
  ylab = "IGD",
  main = "DTLZ7 - Fix -  0.05"
)
dev.off()
pdf(file = "~/pf_0.05.png")
plot(
  PF,
  ylab = "Y",
  xlab = "X",
  main = paste("DTLZ7 - Fix -  0.05 \n Final igd: ", my_igd[1, ])
)
dev.off()
####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----
moea <- moead.random_fixed.0.1
n.iter <- as.numeric(moea$n.iter)
my_igd <- data.frame()
n.iter <- n.iter - 1
for (i in 0:n.iter) {
  idxs <- ((350 * i) + 1):(350 * (i + 1))
  PF <-
    cbind(moea$plot.paretofront$f1[idxs], moea$plot.paretofront$f2[idxs])
  colnames(PF) <- c("V1", "V2")
  igd <- calcIGD(Y = PF,x Yref = Yref)
  my_igd <- rbind(igd, my_igd)
}
png(file = "~/igd_0.1.png")
plot(
  x = nrow(my_igd):1,
  y = my_igd$X,
  xlab = "iteration",
  ylab = "IGD",
  main = "DTLZ7 - Fix -  0.10"
)
dev.off()
png(file = "~/pf_0.1.png")
plot(
  PF,
  ylab = "Y",
  xlab = "X",
  main = paste("DTLZ7 - Fix -  0.10 \n Final igd: ", my_igd[1, ])
)
dev.off()
####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----
moea <- moead.random_fixed.0.15
n.iter <- as.numeric(moea$n.iter)
my_igd <- data.frame()
n.iter <- n.iter - 1
for (i in 0:n.iter) {
  idxs <- ((350 * i) + 1):(350 * (i + 1))
  PF <-
    cbind(moea$plot.paretofront$f1[idxs], moea$plot.paretofront$f2[idxs])
  colnames(PF) <- c("V1", "V2")
  igd <- calcIGD(Y = PF, Yref = Yref)
  my_igd <- rbind(igd, my_igd)
}

plot(
  x = nrow(my_igd):1,
  y = my_igd$X,
  xlab = "iteration",
  ylab = "IGD",
  main = "DTLZ7 - Fix -  0.15"
)
plot(
  PF,
  ylab = "Y",
  xlab = "X",
  main = paste("DTLZ7 - Fix -  0.15 \n Final igd: ", my_igd[1, ])
)
####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----
moea <- moead.random_fixed.0.2
n.iter <- as.numeric(moea$n.iter)
my_igd <- data.frame()
n.iter <- n.iter - 1
for (i in 0:n.iter) {
  idxs <- ((350 * i) + 1):(350 * (i + 1))
  PF <-
    cbind(moea$plot.paretofront$f1[idxs], moea$plot.paretofront$f2[idxs])
  colnames(PF) <- c("V1", "V2")
  igd <- calcIGD(Y = PF, Yref = Yref)
  my_igd <- rbind(igd, my_igd)
}

plot(
  x = nrow(my_igd):1,
  y = my_igd$X,
  xlab = "iteration",
  ylab = "IGD",
  main = "DTLZ7 - Fix -  0.20"
)
plot(
  PF,
  ylab = "Y",
  xlab = "X",
  main = paste("DTLZ7 - Fix -  0.20 \n Final igd: ", my_igd[1, ])
)
####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----
moea <- moead.random_fixed.0.4
n.iter <- as.numeric(moea$n.iter)
my_igd <- data.frame()
n.iter <- n.iter - 1
for (i in 0:n.iter) {
  idxs <- ((350 * i) + 1):(350 * (i + 1))
  PF <-
    cbind(moea$plot.paretofront$f1[idxs], moea$plot.paretofront$f2[idxs])
  colnames(PF) <- c("V1", "V2")
  igd <- calcIGD(Y = PF, Yref = Yref)
  my_igd <- rbind(igd, my_igd)
}

plot(
  x = nrow(my_igd):1,
  y = my_igd$X,
  xlab = "iteration",
  ylab = "IGD",
  main = "DTLZ7 - Fix -  0.40"
)
plot(
  PF,
  ylab = "Y",
  xlab = "X",
  main = paste("DTLZ7 - Fix -  0.40 \n Final igd: ", my_igd[1, ])
)
####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----
moea <- moead.random_fixed.0.6
n.iter <- as.numeric(moea$n.iter)
my_igd <- data.frame()
n.iter <- n.iter - 1
for (i in 0:n.iter) {
  idxs <- ((350 * i) + 1):(350 * (i + 1))
  PF <-
    cbind(moea$plot.paretofront$f1[idxs], moea$plot.paretofront$f2[idxs])
  colnames(PF) <- c("V1", "V2")
  igd <- calcIGD(Y = PF, Yref = Yref)
  my_igd <- rbind(igd, my_igd)
}

plot(
  x = nrow(my_igd):1,
  y = my_igd$X,
  xlab = "iteration",
  ylab = "IGD",
  main = "DTLZ7 - Fix -  0.60"
)
plot(
  PF,
  ylab = "Y",
  xlab = "X",
  main = paste("DTLZ7 - Fix -  0.60 \n Final igd: ", my_igd[1, ])
)
####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----


moea <- moead.random_fixed.0.8
n.iter <- as.numeric(moea$n.iter)
my_igd <- data.frame()
n.iter <- n.iter - 1
for (i in 0:n.iter) {
  idxs <- ((350 * i) + 1):(350 * (i + 1))
  PF <-
    cbind(moea$plot.paretofront$f1[idxs], moea$plot.paretofront$f2[idxs])
  colnames(PF) <- c("V1", "V2")
  igd <- calcIGD(Y = PF, Yref = Yref)
  my_igd <- rbind(igd, my_igd)
}

plot(
  x = nrow(my_igd):1,
  y = my_igd$X,
  xlab = "iteration",
  ylab = "IGD",
  main = "DTLZ7 - Fix -  0.80"
)
plot(
  PF,
  ylab = "Y",
  xlab = "X",
  main = paste("DTLZ7 - Fix -  0.80 \n Final igd: ", my_igd[1, ])
)
####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----



plot(
  x = nrow(my_igd):1,
  y = my_igd$X,
  xlab = "iteration",
  ylab = "IGD",
  main = "DTLZ7 - Fix -  1.0"
)
plot(
  PF,
  ylab = "Y",
  xlab = "X",
  main = paste("DTLZ7 - Fix -  1.0 \n Final igd: ", my_igd[1, ])
)
####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----




#
# # ggsave(filename = "~/Desktop/pareto_fronts.eps", dpi = 500)



# 
# ####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----####
# 
# plots.data <-
#   rbind(
#     cbind(fixed.0.1, Strategy = "10%"),
#     cbind(fixed.0.2, Strategy = "20%"),
#     cbind(fixed.0.4, Strategy = "40%"),
#     cbind(fixed.0.6, Strategy = "60%"),
#     cbind(fixed.0.8, Strategy = "80%"),
#     cbind(fixed.1.0, Strategy = "100%")
#   )
# v <-
#   ggplot(plots.data, aes(f1, f2)) + geom_point(aes(color = Strategy)) + geom_point(aes(shape = Strategy, color = Strategy), size =
#                                                                                      1) + theme(
#                                                                                        legend.text = element_text(),
#                                                                                        axis.text = element_text(),
#                                                                                        axis.title =
#                                                                                          element_text()
#                                                                                      ) + theme(plot.title = element_text(color = "blue",
#                                                                                                                          face = "bold"))
# v + scale_colour_brewer(palette = "Dark2")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----####-----####
# 