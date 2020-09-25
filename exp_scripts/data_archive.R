# rm(list = ls(all = TRUE))
library(smoof)
library(emoa)
library(feather)
library(compiler)
library(ggplot2)
library(ggthemes)
library(MOEADps)


number.fun <- 1
repetitions <- 10
lambda <- 50

fun.names1 <- list()

for (i in 1:7) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}
for (i in 1:7) {
  fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
}

#number_subproblems <- c(3, 4, 5, 6, 7, 8, 9, 10, 30, 50, 100, 150, 250)

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
          name = paste0(fun, "_moead.3_", lambda, "_"),
          wd = "~/jsec_2020_50/",
          j = j
        )
      
      moead500 <-
        loadPlotData(
          name = paste0(fun, "_moead500_", lambda, "_"),
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
      name = paste0(fun, "_moead.random_", lambda, "_"),
      wd = "~/jsec_2020_50/",
      j = 1
    )
  
  
  # moead.smallT <-
  #   loadPlotData(name = paste0(fun, "_moead.smallT_", lambda, "_"), wd = "~/jsec_2020_50/",
  #                j = j)
  
  
  moead3 <-
    loadPlotData(
      name = paste0(fun, "_moead.3_", lambda, "_"),
      wd = "~/jsec_2020_50/",
      j = 1
    )
  
  moead500 <-
    loadPlotData(
      name = paste0(fun, "_moead500_", lambda, "_"),
      wd = "~/jsec_2020_50/",
      j = 1
    )
  
  plot.data3 <-
    rbind(
      data.frame(moead3$Y[ecr::nondominated(t(moead3$Y)), ], Strategy = "Small population"),
      data.frame(moead500$Y[ecr::nondominated(t(moead500$Y)), ], Strategy = "Big population"),
      data.frame(moead.random$Y[ecr::nondominated(t(moead.random$Y)), ], Strategy = "MOEA/D PS = 50")
    )
  plot <- ggplot(plot.data3, aes(f1, f2, size = 18)) +
    geom_point(aes(color = Strategy, shape = Strategy)) +
    theme_minimal(base_size = 26) + scale_colour_hc()
  plot <- plot +  theme(axis.text = element_text(size =24))
  print(plot + theme(legend.position = "bottom", legend.title = element_blank())+guides(size = FALSE, shape = guide_legend(override.aes = list(size = 5))
  ))
  filename = paste0("~/jsec_2020_50/PFs/", fun, ".eps")
  ggsave(
    filename = filename,
    dpi = 1200,
    width = 12,
    height = 12
  )
  
  
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
          name = paste0(fun, "_moead.3_", lambda, "_"),
          wd = "~/jsec_2020_50/",
          j = j
        )
      
      moead500 <-
        loadPlotData(
          name = paste0(fun, "_moead500_", lambda, "_"),
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
          name = paste0("MOEA/D with PS = 50")
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
print(aggregate(results$hv, mean, by = list(results$name, results$fun)))
print(aggregate(results$igd, mean, by = list(results$name, results$fun)))
print(aggregate(results$nndom, mean, by = list(results$name, results$fun)))
write_feather(results, "~/jsec_2020_50/jpnsec_results")