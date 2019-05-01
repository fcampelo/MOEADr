# rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
library(stringr)
library(ecr)
library(mco)
library(feather)
library(pracma)
library(withr)
library(ggplot2)


Resources <- Reduce("+", moead.rad$usage)
Subproblem <- 1:length(Resources)
b <- data.frame(Resources, Subproblem)
v <- ggplot(b, aes(Subproblem, Resources)) +geom_line()+ theme(axis.text = element_text(size =
                                                                                          14),
                                                               axis.title =
                                                                 element_text(size = 16, face = "bold")) + theme(plot.title = element_text(
                                                                   color = "blue",
                                                                   size = 24,
                                                                   face = "bold"
                                                                 ))+ geom_hline(yintercept = 200, colour = "red")+ ylim(0,400)
v +labs(title=paste0("MOEA/D-RAD - F44"))


Resources <- Reduce("+", moead.dra$usage)
Subproblem <- 1:length(Resources)
b <- data.frame(Resources, Subproblem)
v <- ggplot(b, aes(Subproblem, Resources)) +geom_line()+ theme(axis.text = element_text(size =
                                                                                          14),
                                                               axis.title =
                                                                 element_text(size = 16, face = "bold")) + theme(plot.title = element_text(
                                                                   color = "blue",
                                                                   size = 24,
                                                                   face = "bold"
                                                                 ))+ geom_hline(yintercept = 200, colour = "red")+ ylim(0,400)
v +labs(title=paste0("MOEA/D-DRA - F44"))
