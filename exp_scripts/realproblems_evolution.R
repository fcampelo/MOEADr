rm(list = ls(all = TRUE))
library(emoa)
library(feather)
library(ggplot2)
library(ggthemes)
# library(eaf)

repetitions <- 6

checkpoints <- (0:40) * 2500
# checkpoints <- (0:10) * 2500
checkpoints[1] <- 300


fun.names1 <- list()
fun.names1[[1]] <- "moon"
fun.names1[[1]] <- "mazda"



source("~/MOEADr/R/summary_moeadps.R")
source("~/MOEADr/R/utils.R")

strategy <-
  c("MOEA/D",
    "MOEA/D-PS",
    "PS & Updates",
    "Updates")

names <- c("moead",
           "random",
           "random_updates",
           "moead.updates")



# fun_hv <- data.frame()

for (fun in fun.names1) {
  print(fun)
  
  if (fun == "moon") {
    ref.point <- c(1, 0, 1)
    dimensions <- 2
    n.obj <- 3
    pop.size <- 300
  }
  if (fun == "mazda") {
    ref.point <- c(0,1.1)
    dimensions <- 222
    
    n.obj <- 2
  }
  
  total_hv <- data.frame()
  
  stg_idx <- 1
  for (name in names) {
    my_hv <- data.frame()
    print(names)
    print(strategy[stg_idx])
    for (my_rep in 1:repetitions) {
      ck_idx <- 1
      for (nfe.check in checkpoints) {
        if (name == "random" || name == "random_updates") {
          iter <- floor(((nfe.check - 300) / 30))
        }
        else{
          iter <- floor(((nfe.check - 300) / 300))
        }
        iter <- iter + 1 
        
          if (name == "moead") {
            moea <-
              read_feather(paste0(
                "~/gecco2021/",
                fun,
                "_moead_iter_",
                my_rep,
                "/iter_",
                iter
              ))
          }
          else if (name == "random") {
            moea <-
              read_feather(paste0(
                "~/gecco2021/",
                fun,
                "_random_iter_",
                my_rep,
                "/iter_",
                iter
              ))
          }
          else if(name == "moead.updates"){
            moea <-
              read_feather(paste0(
                "~/gecco2021/",
                fun,
                "_moead.updates_iter_",
                my_rep,
                "/iter_",
                iter
              ))
          }
          else if(name == "random_updates"){
            moea <-
              read_feather(paste0(
                "~/gecco2021/",
                fun,
                "_random_updates_iter_",
                my_rep,
                "/iter_",
                iter
              ))
          }
    
          
          moea$Y <-
            moea[, (dimensions + 1):(dimensions + n.obj)]
          PF <- moea$Y
          PF$v <- moea$v
        
        
        
        if (fun == "mazda") {
          colnames(PF) <- c("f1", "f2", "v")
          PF$f1 <- PF$f1 / 74
          PF$f2 <- PF$f2 - 2.0
          
        }
        else{
          colnames(PF) <- c("f1", "f2", "f3", "v")
        }
        if (dim(PF[PF$v == 0, ])[1] == 0) {
          hv <- 0
        }
        else{
          hv <- dominated_hypervolume(t(PF[PF$v == 0, 1:n.obj]), ref = ref.point)
        }
        
        my_hv <-
          rbind(my_hv,
                cbind(
                  hv = hv,
                  iter = nfe.check,
                  fun = fun,
                  Strategy = strategy[stg_idx]
                ))
        if (checkpoints[ck_idx] == checkpoints[length(checkpoints)])
          break
        
        ck_idx <- ck_idx + 1
      }
    }
    
    total_hv <- rbind(total_hv, my_hv)
    stg_idx <- stg_idx + 1
  }
  # fun_hv <-
  # rbind(fun_hv, total_hv)
  
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
    guides(colour = guide_legend(override.aes = list(size=3,linetype=0))) +
    # geom_ribbon(aes(ymin = ymax, ymax = ymin),
    #             alpha = .2,
    #             linetype = 0) +
    geom_line(aes(color = Strategy)) +
    geom_point(aes(color = Strategy, shape = Strategy)) +
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
    v + theme(legend.position = "bottom", legend.title = element_blank()) #+
      # guides(size = FALSE, shape = guide_legend(override.aes = list(size = 1)))
  )
  
  
  filename = paste0("~/gecco2021/", fun , "hv_evolution.png")
  ggsave(
    filename = filename,
    dpi = 300,
    width = 12,
    height = 12
  )
  
  # fun_hv$hv <- as.numeric(as.character(fun_hv$hv))
  # fun_igd$igd <- as.numeric(as.character(fun_igd$igd))
  
  
  # write_feather(fun_hv, "~/gecco2021/fun_hv")
  # write_feather(fun_igd, "fun_igd")
  
}
