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
repetitions <- 10

checkpoints <- (0:80) * 750
checkpoints[1] <- 1
jpnsec_results <- read_feather("~/jsec_2020_50/inv_jpnsec_results")

fun.names1 <- list()

# for (i in 1:7) {
#   fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
# }
for (i in 1:4) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}


#number_subproblems <-
#  c(3, 4, 5, 6, 7, 8, 9, 10, 80, 50, 100, 150, 250)
source("~/MOEADr/R/summary_moead.R")
source("~/MOEADr/R/utils.R")
source("~/MOEADr/R/loadPlotData.R")

strategy <-
  c("Small Pop.",
    "Big Pop.",
    "MOEA/D-PS")

names <- c("moead.3",
           "moead500",
           "moead.random"
           )

results <- data.frame()
for (fun in fun.names1) {
  ref1 <- data.frame()
  print(fun)
  
  benchmark <- strsplit(fun, "[0-9]")[[1]][1]
  number <- strsplit(fun, "[A-Z]")[[1]][3]
  if (benchmark == "DTLZ") {
    Yref <- read.csv(file = paste0("~/MOEADr/inst/extdata/pf_data/inv_",fun,".csv"), header = F)
    colnames(Yref) <- c("f1", "f2")
    ref.point <- c(1, 1)
    number_subproblems <-
      c(50)
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
      number_subproblems <-
        c(50)
    }
    else{
      colnames(Yref) <- c("f1", "f2")
      ref.point <- c(1, 1)
      number_subproblems <-
        c(50)
    }
  }
  
  
  
  
  
  for (j in 1:repetitions) {
    for (lambda in number_subproblems) {
      moead.random <-
        loadPlotData(
          name = paste0("inv_", fun, "_moead.random_", lambda, "_"),
          wd = "~/jsec_2020_50/",
          j = j
        )
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
  
  fun_hv <- data.frame()
  fun_igd <- data.frame()
  fun_igd_plus <- data.frame()
  fun_epsilon <- data.frame()
  stg_idx <- 1
  for (name in names) {
    print(name)
    total_hv <- data.frame()
    total_igd <- data.frame()
    total_igd_plus <- data.frame()
    total_epsilon <- data.frame()
    for (my_rep in 1:repetitions) {
      moea <-
        loadPlotData(
          name = paste0("inv_", fun, "_", name, "_", lambda, "_"),
          j = my_rep,
          wd = "~/jsec_2020_50/"
        )
      moea$n.iter <- as.integer(moea$n.iter)
      my_hv <- data.frame()
      my_igd <- data.frame()
      my_igd_plus <- data.frame()
      my_epsilon <- data.frame()
      
      my_hv <-
        rbind(my_hv, cbind(hv = 0, iter = 0))
      if (name == "moead500") {
        evals <- seq(1, moea$n.iter, 1)
      }
      else{
        evals <- c(seq(1, moea$n.iter, 10), moea$n.iter)
      }
      
      for (iter in evals) {
        PF <-
          data.frame(cbind(
            moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V1,
            moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V2
          ))
        
        if (iter == 1) {
          nfe <- dim(moea$X)[1]
        }
        else{
          if (name == names[2]) {
            #500
            nfe <- (iter) * 500 + 500
          }
          if (name == names[1]) {
            #50
            nfe <- (iter) * 50 + 50
          }
          if (name == names[3]) {
            #PS = 50
            nfe <- (iter) * 50 + 500
          }
        }
        
        
        # calc HV
        colnames(PF) <- colnames(ref1)
        PF <- scaling_Y(PF, ref1)
        hv <- dominated_hypervolume(t(PF), ref = ref.point)
        
        my_hv <-
          rbind(my_hv, cbind(hv = hv, iter = nfe))
        my_igd <-
          rbind(my_igd, cbind(igd = igd(PF, Yref), iter = nfe))
        my_igd_plus <-
          rbind(my_igd_plus, cbind(igd_plus = igd_plus(PF, Yref), iter = nfe))
        my_epsilon <-
          rbind(my_epsilon, cbind(epsilon = epsilon_additive(PF, Yref), iter = nfe))
        
      }
      
      total_hv <- rbind(total_hv, my_hv)
      total_igd <- rbind(total_igd, my_igd)
      total_igd_plus <- rbind(total_igd_plus, my_igd_plus)
      total_epsilon <- rbind(total_epsilon, my_epsilon)
    }
    
    
    fun_hv <-
      rbind(fun_hv, cbind(total_hv, strategy = strategy[stg_idx]))
    
    fun_igd <-
      rbind(fun_igd, cbind(total_igd, strategy = strategy[stg_idx]))
    fun_igd_plus <-
      rbind(fun_igd_plus, cbind(total_igd_plus, strategy = strategy[stg_idx]))
    
    fun_epsilon <-
      rbind(fun_epsilon, cbind(total_epsilon, strategy = strategy[stg_idx]))
    stg_idx <- stg_idx + 1
    
  }

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
    # geom_line(aes(color = Strategy, size = 500)) +
    theme_minimal() + scale_colour_hc() +
    labs(x = "Number of Function Evalutions", y = "HV") + theme_minimal(base_size = 20) +
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
  
  filename = paste0("~/jsec_2020_50/hv_evolution/inv_",
                    fun ,
                    "_hv_evolution.eps")
  ggsave(
    filename = filename,
    dpi = 300,
    width = 14,
    height = 14
  )

 ## IGD
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
    # geom_ribbon(aes(ymin = ymax, ymax = ymin),
    #             alpha = .2,
    #             linetype = 0) +
    
    geom_point(aes(
      color = Strategy,
      shape = Strategy,
      size = 2000
    )) +
    # geom_line(aes(color = Strategy, size = 500)) +
    theme_minimal() + scale_colour_hc() +
    labs(x = "Number of Function Evalutions", y = "igd") + theme_minimal(base_size = 20) +
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
  
  filename = paste0("~/jsec_2020_50/igd_evolution/inv_",
                    fun ,
                    "_igd_evolution.eps")
  ggsave(
    filename = filename,
    dpi = 300,
    width = 14,
    height = 14
  )
  
  ## igd_plus plus
  median <-
    aggregate(fun_igd_plus$igd_plus, median, by = list(fun_igd_plus$strategy, fun_igd_plus$iter))
  sd <-
    aggregate(fun_igd_plus$igd_plus, sd, by = list(fun_igd_plus$strategy, fun_igd_plus$iter))
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
    # geom_line(aes(color = Strategy, size = 500)) +
    theme_minimal() + scale_colour_hc() +
    labs(x = "Number of Function Evalutions", y = "igd_plus") + theme_minimal(base_size = 20) +
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
  
  filename = paste0("~/jsec_2020_50/igd_plus_evolution/inv_",
                    fun ,
                    "_igd_plus_evolution.eps")
  ggsave(
    filename = filename,
    dpi = 300,
    width = 14,
    height = 14
  )
  
  ## epsilon plus
  median <-
    aggregate(fun_epsilon$epsilon, median, by = list(fun_epsilon$strategy, fun_epsilon$iter))
  sd <-
    aggregate(fun_epsilon$epsilon, sd, by = list(fun_epsilon$strategy, fun_epsilon$iter))
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
    # geom_line(aes(color = Strategy, size = 500)) +
    theme_minimal() + scale_colour_hc() +
    labs(x = "Number of Function Evalutions", y = "epsilon") + theme_minimal(base_size = 20) +
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
  
  filename = paste0("~/jsec_2020_50/epsilon_evolution/inv_",
                    fun ,
                    "_epsilon_evolution.eps")
  ggsave(
    filename = filename,
    dpi = 300,
    width = 14,
    height = 14
  )
}