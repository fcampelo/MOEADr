# rm(list = ls(all = TRUE))
library(smoof)
library(emoa)
library(feather)
library(compiler)
library(ggplot2)
library(ggthemes)
library(MOEADps)
library(xtable)

number.fun <- 1
repetitions <- 10
lambda <- 50

fun.names1 <- list()

for (i in 1:7) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}
#number_subproblems <- c(3, 4, 5, 6, 7, 8, 9, 10, 80, 50, 100, 150, 250)

source("~/MOEADr/R/summary_moead.R")
source("~/MOEADr/R/utils.R")
source("~/MOEADr/R/loadPlotData.R")

results <- data.frame()
for (fun in fun.names1) {
  ref1 <- data.frame()
  print(fun)
  
  benchmark <- strsplit(fun, "[0-9]")[[1]][1]
  number <- strsplit(fun, "[A-Z]")[[1]][3]
  if (benchmark == "DTLZ") {
    Yref <-
      as.matrix(read.table(paste0(
        "~/MOEADr/inst/extdata/pf_data/", fun, ".2D.pf"
      )))
    colnames(Yref) <- c("f1", "f2")
    ref.point <- c(1, 1)
    number_subproblems <- 50
  }
  else {
    Yref <-
      as.matrix(read.table(paste0(
        "~/MOEADr/inst/extdata/pf_data/", fun, ".dat"
      )))
    if (as.numeric(number) == 8 ||
        as.numeric(number) == 9 || as.numeric(number) == 10) {
      colnames(Yref) <- c("f1", "f2", "f3")
      ref.point <- c(1, 1, 1)
      number_subproblems <- 50
    }
    else{
      colnames(Yref) <- c("f1", "f2")
      ref.point <- c(1, 1)
      number_subproblems <- 50
    }
  }
  
  
  
  
  
  for (j in 1:repetitions) {
    for (lambda in number_subproblems) {
      moead.random <-
        loadPlotData(
          name = paste0(fun, "_moead.random_", lambda, "_"),
          wd = "~/jsec_2020_50/",
          j = j
        )
      
      
      # moead.smallT <-
      #   loadPlotData(name = paste0(fun, "_moead.smallT_", lambda, "_"), wd = "~/jsec_2020_50/",
      #                j = j)
      
      
      moead3 <-
        loadPlotData(
          name = paste0("inv_", fun, "_moead.3_", lambda, "_"),
          wd = "~/jsec_2020_50/",
          j = j
        )
      
      moead500 <-
        loadPlotData(
          name = paste0("inv_", fun, "_moead500_", lambda, "_"),
          wd = "~/jsec_2020_50/",
          j = j
        )
      ref1 <-
        rbind(ref1,
              moead.random$Y,
              moead3$Y,
              moead500$Y)
    }
  }
  
  moead.random <-
    loadPlotData(
      name = paste0("inv_", fun, "_moead.random_", lambda, "_"),
      wd = "~/jsec_2020_50/",
      j = 1
    )
  
  
  # moead.smallT <-
  #   loadPlotData(name = paste0(fun, "_moead.smallT_", lambda, "_"), wd = "~/jsec_2020_50/",
  #                j = j)
  
  
  moead3 <-
    loadPlotData(
      name = paste0("inv_", fun, "_moead.3_", lambda, "_"),
      wd = "~/jsec_2020_50/",
      j = 1
    )
  
  moead500 <-
    loadPlotData(
      name = paste0("inv_", fun, "_moead500_", lambda, "_"),
      wd = "~/jsec_2020_50/",
      j = 1
    )
  
  plot.data3 <-
    rbind(
      data.frame(moead3$Y[ecr::nondominated(t(moead3$Y)), ], Strategy = "Small pop."),
      data.frame(moead500$Y[ecr::nondominated(t(moead500$Y)), ], Strategy = "Big pop."),
      data.frame(moead.random$Y[ecr::nondominated(t(moead.random$Y)), ], Strategy = "MOEA/D-PS")
    )
  plot <- ggplot(plot.data3, aes(f1, f2, size = 2000)) +
    geom_point(aes(color = Strategy, shape = Strategy)) +
    theme_minimal() + scale_colour_hc()
  plot <-
    plot +  theme(axis.text = element_text(size = 40),
               legend.background = element_rect(size = 0.5, linetype = "dotted"))
  print(
    plot + theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 40)
    ) +
      guides(size = FALSE, shape = guide_legend(override.aes = list(size = 5)))
  )
  filename = paste0("~/jsec_2020_50/PFs/inv_", fun, ".eps")
  ggsave(
    filename = filename,
    dpi = 300,
    width = 14,
    height = 14
  )
  
  
  for (j in 1:repetitions) {
    for (lambda in number_subproblems) {
      moead.random <-
        loadPlotData(
          name = paste0("inv_", fun, "_moead.random_", lambda, "_"),
          wd = "~/jsec_2020_50/",
          j = j
        )
      
      
      # moead.smallT <-
      #   loadPlotData(name = paste0(fun, "_moead.smallT_", lambda, "_"), wd = "~/jsec_2020_50/",
      #                j = j)
      
      
      moead3 <-
        loadPlotData(
          name = paste0("inv_", fun, "_moead.3_", lambda, "_"),
          wd = "~/jsec_2020_50/",
          j = j
        )
      
      moead500 <-
        loadPlotData(
          name = paste0("inv_", fun, "_moead500_", lambda, "_"),
          wd = "~/jsec_2020_50/",
          j = j
        )
      
      
      class(moead.random) <- "moead"
      # class(moead.smallT) <- "moead"
      class(moead3) <- "moead"
      class(moead500) <- "moead"
      
      random <-
        data.frame(
          summary.moead(
            moead.random,
            scaling.reference = ref1,
            ref.point = ref.point,
            ref.front = Yref
          ),
          name = paste0("MOEA/D with PS")
        )
      
      # moead.smallT <-
      #   data.frame(
      #     summary.moead(
      #       moead.smallT,
      #       scaling.reference = ref1,
      #       ref.point = ref.point,
      #       ref.front = Yref
      #     ),
      #     name = paste0("SmallNeighborhood_", lambda)
      #   )
      moead3 <-
        data.frame(
          summary.moead(
            moead3,
            scaling.reference = ref1,
            ref.point = ref.point,
            ref.front = Yref
          ),
          name = paste0("Small population")
        )
      
      moead500 <-
        data.frame(
          summary.moead(
            moead500,
            scaling.reference = ref1,
            ref.point = ref.point,
            ref.front = Yref
          ),
          name = paste0("Big population")
        )
      #
      # print(rbind(random,
      #             moead500,
      #             moead3))
      results <-
        rbind(results, cbind(rbind(random,
                                   moead500,
                                   moead3),
                             fun))
      
    }
    
    
    
  }
}
write_feather(results, "~/jsec_2020_50/inv_jpnsec_results")

results <- results[c(2, 5, 7, 8)]
hv <-
  aggregate(results$hv, mean, by = list(results$name, results$fun))
hv$x <- round(hv$x, 2)
hv$sd <-
  round(aggregate(results$hv, sd, by = list(results$name, results$fun))$x, 2)

hv$nndom <-
  round(aggregate(results$nndom, mean, by = list(results$name, results$fun))$x, 2)
hv$nndom_sd <-
  round(aggregate(results$nndom, sd, by = list(results$name, results$fun))$x, 2)

colnames(hv) <- c("Strategy", "MOP", "Mean", "SD", "NDOM", "N_SD")

temp <- data.frame(MOP = c(levels(hv$MOP), levels(hv$MOP)))
temp$d1 <- c(paste0(hv[seq(1, nrow(hv), by = 3), 3], " (", hv[seq(1, nrow(hv), by = 3), 4], ")"),
             paste0(hv[seq(1, nrow(hv), by = 3), 5], " (", hv[seq(1, nrow(hv), by = 3), 6], ")"))
temp$d2 <- c(paste0(hv[seq(2, nrow(hv), by = 3), 3], " (", hv[seq(2, nrow(hv), by = 3), 4], ")"),
             paste0(hv[seq(2, nrow(hv), by = 3), 5], " (", hv[seq(2, nrow(hv), by = 3), 6], ")"))
temp$d3 <- c(paste0(hv[seq(3, nrow(hv), by = 3), 3], " (", hv[seq(3, nrow(hv), by = 3), 4], ")"),
             paste0(hv[seq(3, nrow(hv), by = 3), 5], " (", hv[seq(3, nrow(hv), by = 3), 6], ")"))
colnames(temp) <- c("MOP", levels(hv$Strategy))


print(xtable(temp, digits = 3), type = "latex")


id.5 <- which(results$fun == c("DTLZ5"))
results <- results[-id.5,]
id.6 <- which(results$fun == c("DTLZ6"))
results <- results[-id.6,]
id.7 <- which(results$fun == c("DTLZ7"))
results <- results[-id.7,]

agg_means <- aggregate(results$hv, mean, by = list(results$name, results$fun))
print(pairwise.wilcox.test(agg_means$x, agg_means$Group.1, paired = TRUE, p.adjust.method = "hommel"))
print(agg_means)

