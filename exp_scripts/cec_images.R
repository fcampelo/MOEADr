rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
library(smoof)
library(MOEADr)
library(emoa)
library(feather)
library(withr)
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)
fun.names1 <- list()
source("visualization_tools.R")

repetitions <- 21


results_fixed <- read_feather("~/myMOEADr/results_fixed")

names <- c(
  "moead.random_fixed.1.0",
  "moead.random_fixed.0.8",
  "moead.random_fixed.0.6",
  "moead.random_fixed.0.4",
  "moead.random_fixed.0.2",
  "moead.random_fixed.0.1"
)

strategy <-
  c("100%",
    "80%",
    "60%",
    "40%",
    "20%",
    "10%")


for (i in 1:7) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}

for (i in 1:7) {
  fun.names1[[length(fun.names1) + 1]]  = paste0("UF", i)
}

for (fun in fun.names1) {
  plots.data  <- data.frame()
  ref1 <- data.frame()
  for (i in 1:repetitions) {
    moead.random.1.0 <-
      loadPlotData(name = paste0(fun, names[1]), j = i)
    moead.random.0.8 <-
      loadPlotData(name = paste0(fun, names[2]), j = i)
    moead.random.0.6 <-
      loadPlotData(name = paste0(fun, names[3]), j = i)
    moead.random.0.4 <-
      loadPlotData(name = paste0(fun, names[4]), j = i)
    moead.random.0.2 <-
      loadPlotData(name = paste0(fun, names[5]), j = i)
    moead.random.0.1 <-
      loadPlotData(name = paste0(fun, names[6]), j = i)
    
    ref1 <-
      rbind(
        ref1,
        moead.random.1.0$Y,
        moead.random.0.8$Y,
        moead.random.0.6$Y,
        moead.random.0.4$Y,
        moead.random.0.2$Y,
        moead.random.0.1$Y
      )
  }
  
  
  temp <- results_fixed[results_fixed$fun == fun,]
  i <- 1
  for (name in names) {
    temp2 <- temp[temp$name == name,]
    
    moea <-
      loadPlotData(name = paste0(fun, name),
                   j = which(temp2$igd == median(temp2$igd)))
    
    
    moea.final.iter <-
      moea$plot.paretofront[moea$plot.paretofront$stage == max(moea$plot.paretofront$stage),]
    moea.final.nndom <-
      moea.final.iter[moea.final.iter$`non-dominated` == 1,]
    moea.final.nndom <-
      as.data.frame(cbind(moea.final.nndom$f1, moea.final.nndom$f2))
    names(moea.final.nndom) <- c("f1", "f2")
    moea.final.nndom <- scaling_Y(Y = moea.final.nndom, X = ref1)
    
    plots.data <-
      rbind(plots.data,
            cbind(moea.final.nndom, Strategy = strategy[i]))
    i <- i + 1
  }
  
  v <-
    ggplot(plots.data, aes(f1, f2)) + geom_line(aes(color = Strategy)) + geom_point(aes(shape = Strategy, color = Strategy), size =
                                                                                      2) + theme(
                                                                                        legend.text = element_text(size =
                                                                                                                     18),
                                                                                        axis.text = element_text(size =
                                                                                                                   14 *
                                                                                                                   2),
                                                                                        axis.title =
                                                                                          element_text(size = 14 *
                                                                                                         2, face = "bold")
                                                                                      ) + theme(plot.title = element_text(
                                                                                        color = "blue",
                                                                                        size = 24 *
                                                                                          3,
                                                                                        face = "bold"
                                                                                      ))
  v + scale_colour_brewer(palette = "Dark2")
  filename = paste0("~/images_cec/", fun, "igd_pareto_fronts.eps")
  ggsave(filename = filename, dpi = 600)
  
  
  i <- 1
  for (name in names) {
    temp2 <- temp[temp$name == name,]
    
    moea <-
      loadPlotData(name = paste0(fun, name),
                   j = which(temp2$hv == median(temp2$hv)))
    
    
    moea.final.iter <-
      moea$plot.paretofront[moea$plot.paretofront$stage == max(moea$plot.paretofront$stage),]
    moea.final.nndom <-
      moea.final.iter[moea.final.iter$`non-dominated` == 1,]
    moea.final.nndom <-
      as.data.frame(cbind(moea.final.nndom$f1, moea.final.nndom$f2))
    names(moea.final.nndom) <- c("f1", "f2")
    moea.final.nndom <- scaling_Y(Y = moea.final.nndom, X = ref1)
    
    plots.data <-
      rbind(plots.data,
            cbind(moea.final.nndom, Strategy = strategy[i]))
    i <- i + 1
  }
  
  
  v <-
    ggplot(plots.data, aes(f1, f2)) + geom_line(aes(color = Strategy)) + geom_point(aes(shape = Strategy, color = Strategy), size =
                                                                                      2) + theme(
                                                                                        legend.text = element_text(size =
                                                                                                                     18),
                                                                                        axis.text = element_text(size =
                                                                                                                   14 *
                                                                                                                   2),
                                                                                        axis.title =
                                                                                          element_text(size = 14 *
                                                                                                         2, face = "bold")
                                                                                      ) + theme(plot.title = element_text(
                                                                                        color = "blue",
                                                                                        size = 24 *
                                                                                          3,
                                                                                        face = "bold"
                                                                                      ))
  v + scale_colour_brewer(palette = "Dark2")
  filename = paste0("~/images_cec/", fun, "hv_pareto_fronts.eps")
  ggsave(filename = filename, dpi = 600)
}

