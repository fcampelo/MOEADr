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
number.fun <- 1
repetitions <- 10

checkpoints <- (0:40) * 750
collab_results <- read_feather("collab_results")

for (i in 9:9) {
  fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
}

for (i in 7:7) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}

number_subproblems <-
  c(3, 4, 5, 6, 7, 8, 9, 10, 30, 50, 100, 150, 250)


strategy <-
  c("random",
    "DS",
    "RI")

names <- c("moead.random",
           "moead.DS",
           "moead.RI")

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
    }
    else{
      colnames(Yref) <- c("f1", "f2")
      ref.point <- c(1, 1)
    }
  }
  
  
  
  
  
  for (j in 1:repetitions) {
    for (lambda in number_subproblems) {
      moead.random <-
        loadPlotData(
          name = paste0(fun, "_moead.random_", lambda, "_", wd = "~/france_data/"),
          j = j
        )
      
      
      moead.ds <-
        loadPlotData(
          name = paste0(fun, "_moead.norm_", lambda, "_", wd = "~/france_data/"),
          j = j
        )
      
      
      moead.RI <-
        loadPlotData(
          name = paste0(fun, "_moead.RI_", lambda, "_", wd = "~/france_data/"),
          j = j
        )
      ref1 <-
        rbind(ref1,
              moead.random$Y,
              moead.ds$Y,
              moead.RI$Y)
    }
  }
  
  # for (fun in fun.names1) {
  temp <- collab_results[collab_results$fun == fun, ]
  
  fun_hv <- data.frame()
  stg_idx <- 1
  for (name in names) {
    total_hv <- data.frame()
    for (my_rep in 1:repetitions) {
      
      
      for (lambda in number_subproblems) {
        moea <-
          loadPlotData(
            name = paste0(fun, "_", name, "_", lambda, "_", wd = "~/france_data/"),
            j = my_rep
          )
        ck_idx <- 1
        n.iter <- as.integer(moea$n.iter) - 1
        my_sum <- 0
        my_hv <- data.frame()
        for (j in 0:n.iter) {
          idxs <- ((350 * j) + 1):(350 * (j + 1))
          my_sum <-
            sum(moea$plot.resources$Resources[idxs]) #+ my_sum
          if (my_sum >= checkpoints[ck_idx]) {
            if (as.numeric(number) == 9) {
              PF <-
                cbind(
                  moea$plot.paretofront$f1[idxs],
                  moea$plot.paretofront$f2[idxs],
                  moea$plot.paretofront$f3[idxs]
                )
            }
            else if (as.numeric(number) == 7) {
              PF <-
                cbind(moea$plot.paretofront$f1[idxs],
                      moea$plot.paretofront$f2[idxs])
            }
            
            colnames(PF) <- colnames(ref1)
            PF <- scaling_Y(PF, ref1)
            hv <- dominated_hypervolume(t(PF), ref = ref.point)
            my_hv <-
              rbind(my_hv, cbind(hv = hv, iter = checkpoints[ck_idx]))
            ck_idx <- ck_idx + 1
          }
        }
      }
      total_hv <- rbind(total_hv, my_hv)
    }
    
    fun_hv <-
      rbind(fun_hv, cbind(total_hv, strategy = strategy[stg_idx]))
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
  
  
  # plot_data,
  
  v <- ggplot(data = plot_data,
              aes(
                x = iter,
                y = median,
                group = Strategy,
                color = Strategy,
                fill = Strategy
              )) +
    geom_point(aes(color = Strategy, fill = Strategy)) +
    geom_line(aes(color = Strategy, fill = Strategy)) +
    geom_ribbon(aes(ymin = ymax, ymax = ymin),
                alpha = .2,
                linetype = 0) +
    theme(
      legend.text = element_text(size =
                                   16),
      axis.text = element_text(size =
                                 24),
      axis.title =
        element_text(size = 26, face = "bold")
    )  +
    labs(x = "Number of Function Evalutions", y = "HV")
  # v <- v + scale_colour_brewer(palette = "Dark2")
  filename = paste0("~/hv_evolving/", fun , "hv_evolution.png")
  ggsave(filename = filename,
         dpi = 100,
         width = 9)
  # exit()
  # }
  
}