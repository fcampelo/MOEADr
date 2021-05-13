rm(list = ls(all = TRUE))
library(smoof)
library(emoa)
library(feather)
library(compiler)
library(ggplot2)
library(ggthemes)
library(MOEADps)
library(eaf)
options(kableExtra.latex.load_packages = T)
library(kableExtra)

source("~/MOEADr/R/utils.R")

repetitions <- 10

checkpoints <- (0:5) * 4000
checkpoints[1] <- 500

n.obj <- 3

names <- c("moead", "random")


source("~/MOEADr/R/summary_moeadps.R")
source("~/MOEADr/R/utils.R")
source('~/MOEADr/R/loadPlotData.R')


fun_hv <- data.frame()
results <- data.frame()
stats0 <- data.frame()

# for (fun in fun.names1) {
  ref1 <- data.frame()
  
  ref.point <- c(1.1, 1.1, 1.1)
  
  
  
  for (j in 1:repetitions) {
    for (iter in 1:((30000 - 500) / 500)) {
      moead200 <-
        loadPlotData2(name = paste0(j, "_iter_"),
                      j = iter - 1,
                      wd = "~/Desktop/linPF/moead/")
      ref1 <-
        rbind(ref1,
              moead200$Y)
    }
    
    for (iter in 1:ceiling((30000 - 500) / 50)) {
      moead.ps <-
        loadPlotData2(name = paste0(j, "_iter_"),
                      j = iter - 1,
                      wd = "~/Desktop/linPF/random/")
      
      
      ref1 <-
        rbind(ref1,
              moead.ps$Y)
    }
  }
  
  
  #############################################################################################################################
  #############################################################################################################################
  #############################################################################################################################
  
  
  total_hv <- data.frame()
  
  stg_idx <- 1
  for (name in names) {
    my_hv <- data.frame()
    # print(strategy[stg_idx])
    for (my_rep in 1:repetitions) {
      ck_idx <- 1
      
      
      
      for (nfe.check in checkpoints) {
        
        if(name == "moead"){
          iter <- nfe.check/500
          moea <-
            loadPlotData2(name = paste0(my_rep, "_iter_"),
                          j = iter,
                          wd = paste0("~/Desktop/linPF/",name,"/"))
        }else{
          iter <- floor((nfe.check)/50)
          moea <-
            loadPlotData2(name = paste0(my_rep, "_iter_"),
                          j = iter,
                          wd = paste0("~/Desktop/linPF/",name,"/"))
        }
        
        
        PF <- moea$Y
        colnames(PF) <- colnames(ref1)
        PF <- scaling_Y(PF, ref1)
        hv <- dominated_hypervolume(t(PF), ref = ref.point)
        
        
        
        nndom.idx <- ecr::nondominated(t(PF))
        nndom <- sum(nndom.idx)
        
        my_hv <-
          rbind(my_hv,
                cbind(
                  hv = hv,
                  iter = checkpoints[ck_idx],
                  fun = "linPF6",
                  Strategy = name,
                  nndom = nndom
                ))
        
        
        if (checkpoints[ck_idx] == checkpoints[length(checkpoints)]){
          
          
          
          # results <- rbind(results,
          #                  cbind(
          #                    hv = hv,
          #                    fun = "linPF6",
          #                    name = name,
          #                    nndom = nndom
          #                  ))
          break
        }
        
        
        ck_idx <- ck_idx + 1
      }
      
      
    }
    stats0 <- rbind(stats0, my_hv)
    total_hv <- rbind(total_hv, my_hv)
    stg_idx <- stg_idx + 1
  }
  
  total_hv$hv <- as.numeric(as.character(total_hv$hv))
  median <-
    aggregate(total_hv$hv, mean, by = list(total_hv$Strategy, total_hv$iter))
  sd <-
    aggregate(total_hv$hv, sd, by = list(total_hv$Strategy, total_hv$iter))
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
  v <-
    v + theme(axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ))
  print(
    v + theme(legend.position = "bottom", legend.title = element_blank()) +
      guides(size = FALSE, shape = guide_legend(override.aes = list(size = 5)))
  )
  
  
  # filename = paste0("~/Desktop/linPF/hv_evolution/", fun , "hv_evolution.png")
  # ggsave(
  #   filename = filename,
  #   dpi = 300,
  #   width = 12,
  #   height = 12
  # )
  
  
# }

# write_feather(results, "~/Desktop/linPF/ecj_stats")
# 
# 
# 
# stats0$hv <- as.numeric(levels(stats0$hv))[stats0$hv]
# 
# write_feather(stats0, "~/Desktop/linPF/ecj_stats")
# 
# p.values <- data.frame()
# 
# for (iter in unique(stats0$iter)) {
#   stats1 <- stats0[stats0$iter == iter, ]
#   stats2 <-
#     aggregate(stats1$hv, median, by = list(stats1$Strategy, stats1$fun))
#   colnames(stats2) <- c("Strategy", "fun", "hv")
#   # cat("-=-=-=-=-=-=-=-=-=-=-")
#   # cat("\niter: ",iter)
#   # print(
#   #   pairwise.wilcox.test(
#   #     stats2$hv,
#   #     stats2$Strategy,
#   #     paired = TRUE,
#   #     p.adj = "hommel",
#   #     conf.int = TRUE
#   #   )
#   # )
#   
#   tmp <- pairwise.wilcox.test(
#     stats2$hv,
#     stats2$Strategy,
#     paired = TRUE,
#     p.adj = "hommel",
#     conf.int = TRUE
#   )$p.value
#   
#   tmp <- c(tmp)[-3]
#   
#   p.values <- rbind(p.values, c(tmp, as.numeric(iter)))
#   # colnames(p.values) <- c("PSvsBig", "PSvsSmall", "BigvsSmall", "iter")
#   # cat("-=-=-=-=-=-=-=-=-=-=-")
# }
# 
# tmp2 <- p.values
# p.values <- p.values[-4]
# p.values <- as.data.frame(matrix(t(apply(p.values, FUN = formatC, MARGIN = 1, format = "e", digits = 2)), ncol = 3))
# # p.values <- as.data.frame(matrix(t(round(p.values,3)), ncol = 3))
# 
# p.values[,4] <- tmp2[,4]
# p.values <- p.values[c(4,1,2,3)]
# kbl(p.values, booktabs = T, format = "latex")
# 
# 
