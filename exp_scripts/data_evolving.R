rm(list = ls(all = TRUE))
library(smoof)
library(emoa)
library(feather)
library(compiler)
library(ggplot2)
library(ggthemes)
library(MOEADps)


number.fun <- 1
repetitions <- 10

checkpoints <- (0:40) * 750
checkpoints[1] <- 1
jpnsec_results <- read_feather("~/jsec_2020_50/jpnsec_results")

fun.names1 <- list()

for (i in 1:4) {
  fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
}
for (i in 1:4) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}


#number_subproblems <-
#  c(3, 4, 5, 6, 7, 8, 9, 10, 30, 50, 100, 150, 250)
source("~/MOEADr/R/summary_moead.R")
source("~/MOEADr/R/utils.R")
source("~/MOEADr/R/loadPlotData.R")

strategy <-
  c("MOEA/D with PS=3",
    "Big Population",
    "Small Population")

names <- c("moead.random",
           "moead500",
           "moead.3")

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
          name = paste0(fun, "_moead.random_", lambda, "_"),
          wd = "~/jsec_2020_50/",
          j = j
        )
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
  
  fun_hv <- data.frame()
  fun_igd <- data.frame()
  stg_idx <- 1
  for (name in names) {
    print(name)
    total_hv <- data.frame()
    total_igd <- data.frame()
    for (my_rep in 1:repetitions) {
      moea <-
        loadPlotData(
          name = paste0(fun, "_", name, "_", lambda, "_"),
          j = my_rep,
          wd = "~/jsec_2020_50/"
        )
      moea$n.iter <- as.integer(moea$n.iter)
      my_hv <- data.frame()
      my_igd <- data.frame()
      sampled <-
        sort(seq(to = moea$n.iter, by = ceiling(moea$n.iter / 10)))
      for (iter in sampled) {
        PF <-
          data.frame(cbind(
            moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V1,
            moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V2
          ))
        # calc IGD
        igd <- calcIGD(PF, Yref = Yref)
        my_igd <-
          rbind(my_igd, cbind(igd = igd, iter = iter))
        
        # calc HV
        colnames(PF) <- colnames(ref1)
        PF <- scaling_Y(PF, ref1)
        hv <- dominated_hypervolume(t(PF), ref = ref.point)
        my_hv <-
          rbind(my_hv, cbind(hv = hv, iter = iter))
        
        
      }
      total_hv <- rbind(total_hv, my_hv)
      total_igd <- rbind(total_igd, my_igd)
    }
    
    
    fun_hv <-
      rbind(fun_hv, cbind(total_hv, strategy = strategy[stg_idx]))
    
    fun_igd <-
      rbind(fun_igd, cbind(total_igd, strategy = strategy[stg_idx]))
    stg_idx <- stg_idx + 1

  }
  
  fevals <- seq(to = 30000, by = 2500)
  fevals <- fevals[1:10]
  fun_hv$iter <- fevals
  median <-
    aggregate(fun_hv$hv, median, by = list(fun_hv$strategy, fun_hv$iter))
  sd <-
    aggregate(fun_hv$hv, sd, by = list(fun_hv$strategy, fun_hv$iter))
  plot_data <- (data.frame(median, sd))
  plot_data <- plot_data[,-c(4, 5)]
  colnames(plot_data) <- c("Strategy", "iter", "median", "sd")
  plot_data$ymax <- plot_data$median + plot_data$sd
  plot_data$ymin <- plot_data$median - plot_data$sd
  
  v <- ggplot(data = plot_data,
              aes(
                x = iter,
                y = median,
                group = Strategy,
                color = Strategy,
                fill = Strategy
              )) +
    geom_ribbon(aes(ymin = ymax, ymax = ymin),
                alpha = .2,
                linetype = 0) +
    geom_line(aes(color = Strategy)) +
    theme_minimal(base_size = 26) +
    labs(x = "Number of Function Evalutions", y = "HV") + theme_minimal(base_size = 26) +
    scale_fill_discrete(guide = FALSE)
  v <-
    v +  theme(
      axis.text = element_text(size = 24),
      legend.background = element_rect(size = 0.5, linetype = "solid")
    )
  print(
    v + theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(size = FALSE, shape = guide_legend(override.aes = list(size = 5)))
  )
  
  
  filename = paste0("~/jsec_2020_50/hv_evolving/", fun , "hv_evolution.eps")
  ggsave(
    filename = filename,
    dpi = 1200,
    width = 12,
    height = 12
  )
  
  ## IGD
  fevals <- seq(to = 30000, by = 2500)
  fevals <- fevals[1:10]
  fun_igd$iter <- fevals
  median <-
    aggregate(fun_igd$igd, median, by = list(fun_igd$strategy, fun_igd$iter))
  sd <-
    aggregate(fun_igd$igd, sd, by = list(fun_igd$strategy, fun_igd$iter))
  plot_data <- (data.frame(median, sd))
  plot_data <- plot_data[,-c(4, 5)]
  colnames(plot_data) <- c("Strategy", "iter", "median", "sd")
  plot_data$ymax <- plot_data$median + plot_data$sd
  plot_data$ymin <- plot_data$median - plot_data$sd
  
  v <- ggplot(data = plot_data,
              aes(
                x = iter,
                y = median,
                group = Strategy,
                color = Strategy,
                fill = Strategy
              )) +
    geom_ribbon(aes(ymin = ymax, ymax = ymin),
                alpha = .2,
                linetype = 0) +
    geom_line(aes(color = Strategy)) +
    theme_minimal(base_size = 26) +
    labs(x = "Number of Function Evalutions", y = "IGD") + theme_minimal(base_size = 26) +
    scale_fill_discrete(guide = FALSE)
  v <-
    v +  theme(
      axis.text = element_text(size = 24),
      legend.background = element_rect(size = 0.5, linetype = "solid")
    )
  print(
    v + theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(size = FALSE, shape = guide_legend(override.aes = list(size = 5)))
  )
  
  
  filename = paste0("~/jsec_2020_50/hv_evolving/", fun , "igd_evolution.eps")
  ggsave(
    filename = filename,
    dpi = 1200,
    width = 12,
    height = 12
  )

}
