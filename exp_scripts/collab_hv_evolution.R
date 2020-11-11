rm(list = ls(all = TRUE))
library(smoof)
library(emoa)
library(feather)
library(compiler)
library(ggplot2)
library(ggthemes)
library(MOEADps)
library(eaf)


number.fun <- 1
repetitions <- 9

collab_results_uf9 <-
  read_feather("~/france_data/uf9_collab_results")
collab_results_dtlz7 <-
  read_feather("~/france_data/dtlz7_collab_results")

collab_results <- rbind(collab_results_uf9, collab_results_dtlz7)


fun.names1 <- list()

for (i in 9:9) {
  fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
}

for (i in 7:7) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}



source("~/MOEADr/R/summary_moead.R")
source("~/MOEADr/R/utils.R")
source("~/MOEADr/R/loadPlotData.R")

strategy <-
  c("DS",
    "RI",
    "MOEA/D-PS")

names <- c("moead.norm",
           "moead.RI",
           "moead.random")


results <- data.frame()
for (fun in fun.names1) {
  ref1 <- data.frame()
  print(fun)
  
  benchmark <- strsplit(fun, "[0-9]")[[1]][1]
  number <- strsplit(fun, "[A-Z]")[[1]][3]
  if (benchmark == "DTLZ") {
    Yref <-
      as.matrix(read.table(paste0(
        "../inst/extdata/pf_data/", fun, ".2D.pf"
      )))
    colnames(Yref) <- c("f1", "f2")
    ref.point <- c(1, 1)
    number_subproblems <-
      c(3, 4, 6, 8, 10, 30, 50, 100, 150, 250)
  }
  else {
    Yref <-
      as.matrix(read.table(paste0(
        "../inst/extdata/pf_data/", fun, ".dat"
      )))
    if (as.numeric(number) == 8 ||
        as.numeric(number) == 9 || as.numeric(number) == 10) {
      colnames(Yref) <- c("f1", "f2", "f3")
      ref.point <- c(1, 1, 1)
      number_subproblems <-
        c(4, 6, 8, 10, 30, 50, 100, 150, 250)
    }
    else{
      colnames(Yref) <- c("f1", "f2")
      ref.point <- c(1, 1)
      number_subproblems <-
        c(3, 4, 6, 8, 10, 30, 50, 100, 150, 250)
    }
  }
  
  
  
  
  for (j in 1:repetitions) {
    for (lambda in number_subproblems) {
      moead.random <-
      loadPlotData(
        name = paste0(fun, "_moead.random_", lambda, "_"),
        j = j,
        wd = "~/france_data/"
      )
      
      
      moead.ds <-
        loadPlotData(
          name = paste0(fun, "_moead.norm_", lambda, "_"),
          j = j,
          wd = "~/france_data/"
        )
      
      
      moead.RI <-
        loadPlotData(
          name = paste0(fun, "_moead.RI_", lambda, "_"),
          j = j,
          wd = "~/france_data/"
        )
      ref1 <-
        rbind(ref1,
              moead.random$Y,
              moead.ds$Y,
              moead.RI$Y)
    }
  }
  
  fun_hv <- data.frame()
  fun_igd <- data.frame()
  stg_idx <- 1
  for (name in names) {
    print(name)
    total_hv <- data.frame()
    total_igd <- data.frame()
    for (my_rep in 1:repetitions) {
      for (lambda in number_subproblems) {
        moea <-
          loadPlotData(
            name = paste0(fun, "_", name, "_", lambda, "_"),
            j = my_rep,
            wd = "~/france_data/"
          )
        moea$n.iter <- as.integer(moea$n.iter)
        my_hv <- data.frame()
        my_igd <- data.frame()
        
        # my_hv <-
        #   rbind(my_hv, cbind(hv = 0, iter = 0))
        
        
        evals <- c(seq(1, moea$n.iter, 100), moea$n.iter)
        
        for (iter in evals) {
          if (number == 9) {
            PF <-
              data.frame(
                cbind(
                  moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V1,
                  moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V2,
                  moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V3
                )
              )
          }
          else{
            PF <-
              data.frame(cbind(
                moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V1,
                moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V2
              ))
          }
          
          if (iter == 1) {
            nfe <- dim(moea$X)[1]
          }
          else{
            nfe <- iter * lambda + 500
          }
          
          
          # calc HV
          colnames(PF) <- colnames(ref1)
          
          my_igd <-
            rbind(my_igd, cbind(igd = igd(PF, Yref), iter = nfe, Strategy = paste0(strategy[stg_idx], "_",lambda)))
          
          PF <- scaling_Y(PF, ref1)
          hv <- dominated_hypervolume(t(PF), ref = ref.point)
          
          my_hv <-
            rbind(my_hv, cbind(hv = hv, iter = nfe, Strategy = paste0(strategy[stg_idx], "_",lambda)))
        }
      }
      total_hv <- rbind(total_hv, my_hv)
      total_igd <- rbind(total_igd, my_igd)
    }
    
    
    fun_hv <-
      rbind(fun_hv, total_hv)
    
    fun_igd <-
      rbind(fun_igd, total_igd)
    
    stg_idx <- stg_idx + 1
    
  }
  print(head(fun_hv))
  median <-
    aggregate(fun_hv$hv, median, by = list(fun_hv$strategy, fun_hv$iter))
  sd <-
    aggregate(fun_hv$hv, sd, by = list(fun_hv$strategy, fun_hv$iter))
  plot_data <- (data.frame(median, sd))
  plot_data <- plot_data[,-c(4, 5)]
  colnames(plot_data) <- c("Strategy", "iter", "median", "sd")
  plot_data$ymax <- plot_data$median + plot_data$sd
  plot_data$ymin <- plot_data$median - plot_data$sd
  
  v <- ggplot(
    data = plot_data,
    aes(
      x = iter,
      y = median,
      group = Strategy,
      color = Strategy,
      shape = Strategy,
      fill = Strategy
    )
  ) +
    # geom_ribbon(aes(ymin = ymax, ymax = ymin),
    #             alpha = .2,
    #             linetype = 0) +
    
    geom_point(aes(
      color = Strategy,
      shape = Strategy,
      size = 2000
    )) +
    geom_line(aes(color = Strategy, size = 500)) +
    theme_minimal() + scale_colour_hc() +
    labs(x = "Number of Function Evalutions", y = "HV") + theme_minimal(base_size = 20) +
    scale_fill_discrete(guide = FALSE)
  v <-
    v +  theme(
      axis.text = element_text(size = 40),
      legend.background = element_rect(size = 0.5, linetype = "dotted")
    )
  print(
    v + theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 40)
    ) +
      guides(size = FALSE, shape = guide_legend(override.aes = list(size = 5)))
  )
  
  filename = paste0("~/france_data/metric_evolution/",
                    fun ,
                    "_hv_evolution.png")
  ggsave(
    filename = filename,
    dpi = 300,
    width = 14,
    height = 14
  )
  #
  # ## IGD
  median <-
    aggregate(fun_igd$igd, median, by = list(fun_igd$strategy, fun_igd$iter))
  sd <-
    aggregate(fun_igd$igd, sd, by = list(fun_igd$strategy, fun_igd$iter))
  plot_data <- (data.frame(median, sd))
  plot_data <- plot_data[,-c(4, 5)]
  colnames(plot_data) <- c("Strategy", "iter", "median", "sd")
  plot_data$ymax <- plot_data$median + plot_data$sd
  plot_data$ymin <- plot_data$median - plot_data$sd

  v <- ggplot(
    data = plot_data,
    aes(
      x = iter,
      y = median,
      group = Strategy,
      color = Strategy,
      shape = Strategy,
      fill = Strategy
    )
  ) +
  #   geom_ribbon(aes(ymin = ymax, ymax = ymin),
  #               alpha = .2,
  #               linetype = 0) +

    geom_point(aes(
      color = Strategy,
      shape = Strategy,
      size = 2000
    )) +
    geom_line(aes(color = Strategy, size = 500)) +
    theme_minimal() + scale_colour_hc() +
    labs(x = "Number of Function Evalutions", y = "IGD") + theme_minimal(base_size = 20) +
    scale_fill_discrete(guide = FALSE)
  v <-
    v +  theme(axis.text = element_text(size = 40),
               legend.background = element_rect(size = 0.5, linetype = "dotted"))
  print(
    v + theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 40)
    ) +
      guides(size = FALSE, shape = guide_legend(override.aes = list(size = 5)))
  )

  filename = paste0("~/france_data/metric_evolution/",
                    fun ,
                    "_igd_evolution.png")
  ggsave(
    filename = filename,
    dpi = 300,
    width = 14,
    height = 14
  )
  #
  #
}