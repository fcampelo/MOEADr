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

checkpoints <- (0:40) * 750

ref.point <- c(1, 1, 1)

fun.names1 <- list()
# for (i in 1:7) {
#   fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
# }
for (i in 8:10) {
  fun.names1[[length(fun.names1) + 1]]  = paste0("UF", i)
}

for (fun in fun.names1) {
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
  
  
  # for (fun in fun.names1) {
    temp <- results_fixed[results_fixed$fun == fun, ]
    
    fun_hv <- data.frame()
    stg_idx <- 1
    for (name in names) {
      total_hv <- data.frame()
      for (my_rep in 1:repetitions) {
        moea <-
          loadPlotData(name = paste0(fun, name),
                       j = my_rep)
        
        ck_idx <- 1
        n.iter <- as.integer(moea$n.iter) - 1
        my_sum <- 0
        my_hv <- data.frame()
        for (j in 0:n.iter) {
          idxs <- ((350 * j) + 1):(350 * (j + 1))
          my_sum <-
            sum(moea$plot.resources$Resources[idxs]) #+ my_sum
          if (my_sum >= checkpoints[ck_idx]) {
            PF <-
              cbind(moea$plot.paretofront$f1[idxs],
                    moea$plot.paretofront$f2[idxs],#)
                    moea$plot.paretofront$f3[idxs])
            colnames(PF) <- colnames(ref1)
            PF <- scaling_Y(PF, ref1)
            hv <- dominated_hypervolume(t(PF), ref = ref.point)
            my_hv <-
              rbind(my_hv, cbind(hv = hv, iter = checkpoints[ck_idx]))
            ck_idx <- ck_idx + 1
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
      geom_point(aes(color = Strategy,fill = Strategy)) + 
      geom_line(aes(color = Strategy,fill = Strategy)) + 
      geom_ribbon(aes(ymin =ymax, ymax = ymin),alpha = .2, linetype = 0) +
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
