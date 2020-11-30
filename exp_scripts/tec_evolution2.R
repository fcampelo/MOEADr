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

checkpoints <- (0:10) * 10000
checkpoints[1] <- 500


fun.names1 <- list()

for (i in 1:55) {
  fun.names1[[length(fun.names1) + 1]] = paste0("bibbob_", i)
}

for (i in 1:10) {
  fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
}


for (i in 1:7) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}


for (i in 1:4) {
  fun.names1[[length(fun.names1) + 1]] = paste0("inv_DTLZ", i)
}



source("~/MOEADr/R/summary_moeadps.R")
source("~/MOEADr/R/utils.R")
# source("~/MOEADr/R/loadPlotData.R")
#
strategy <-
  c("MOEA/D-PS",
    "Big pop.",
    "Small pop.")

names <- c("moead.ps.50",
           "moead500",
           # "_moead.ps.1_50",
           "moead50")

lambda <- 50


fun_hv <- data.frame()
# fun_igd <- data.frame()
results <- data.frame()

for (fun in fun.names1) {
  ref1 <- data.frame()
  print(fun)
  
  benchmark <- strsplit(fun, "[0-9]")[[1]][1]
  number <- strsplit(fun, "[A-Z]")[[1]][3]
  if (is.na(number))
    number <- -1

  if (benchmark == "DTLZ") {
    # Yref <-
    #   as.matrix(read.table(paste0(
    #     "../inst/extdata/pf_data/", fun, ".2D.pf"
    #   )))
    # colnames(Yref) <- c("f1", "f2")
    ref.point <- c(1, 1)
    # number_subproblems <-
    #   c(3, 4, 6, 8, 10, 30, 50, 100, 150, 250)
  }
  if (benchmark == "UF") {
    #   Yref <-
    #     as.matrix(read.table(paste0(
    #       "../inst/extdata/pf_data/", fun, ".dat"
    #     )))
    if (as.numeric(number) == 8 ||
        as.numeric(number) == 9 || as.numeric(number) == 10) {
      # colnames(Yref) <- c("f1", "f2", "f3")
      ref.point <- c(1, 1, 1)
      # number_subproblems <-
      #   c(4, 6, 8, 10, 30, 50, 100, 150, 250)
    }
    else{
      # colnames(Yref) <- c("f1", "f2")
      ref.point <- c(1, 1)
      # number_subproblems <-
      #   c(3, 4, 6, 8, 10, 30, 50, 100, 150, 250)
    }
  }
  if (benchmark == "bibbob_") {
    ref.point <- c(1, 1)
    # Yref <-
    #   as.matrix(read.table(paste0("../inst/extdata/pf_data/UF2.dat")))
    # colnames(Yref) <- c("f1", "f2")
  }
  
  
  
  
  for (j in 1:repetitions) {
    # for (lambda in number_subproblems) {
    
    moead50 <-
      loadPlotData(
        name = paste0(fun, "_moead50_", lambda, "_"),
        j = j,
        wd = "~/tec/"
      )
    
    # exit()
    moead500 <-
      loadPlotData(
        name = paste0(fun, "_moead500_", lambda, "_"),
        j = j,
        wd = "~/tec/"
      )
    
    moead.ps.50 <-
      loadPlotData(
        name = paste0(fun, "_moead.ps.50_", lambda, "_"),
        j = j,
        wd = "~/tec/"
      )
    ref1 <-
      rbind(ref1,
            moead50$Y,
            moead500$Y,
            moead.ps.50$Y)
  }
  
  
  total_hv <- data.frame()
  # total_igd <- data.frame()
  
  # my_igd <- data.frame()
  
  stg_idx <- 1
  for (name in names) {
    print(strategy[stg_idx])
    for (my_rep in 1:repetitions) {
      moea <-
        loadPlotData(
          name = paste0(fun, "_", name, "_", lambda, "_"),
          j = my_rep,
          wd = "~/tec/"
        )
      moea$n.iter <- as.integer(moea$n.iter)
      
      ck_idx <- 1
      my_hv <- data.frame()
      for (nfe in checkpoints) {
        
        PF <- data.frame()
        time.frame <- 0
        iter <- nfe / 500
        
        # MOEA/D with 50 solutions -> UEA
        if (name == names[3]) {
          time.frame <- 9
          iter <- nfe / 50
        }
        if(iter > 1) iter <- iter - 1
        
        for (n in 0:time.frame) {
          if (as.numeric(number) == 8 ||
              as.numeric(number) == 9 || as.numeric(number) == 10) {
            PF <- rbind(
              PF,
              cbind(
                moea$plot.paretofront[moea$plot.paretofront$stage == iter + n,]$V1,
                moea$plot.paretofront[moea$plot.paretofront$stage == iter + n,]$V2,
                moea$plot.paretofront[moea$plot.paretofront$stage == iter + n,]$V3
              )
            )
          }
          
          else{
            PF <- rbind(
              PF,
              cbind(
                moea$plot.paretofront[moea$plot.paretofront$stage == iter + n,]$V1,
                moea$plot.paretofront[moea$plot.paretofront$stage == iter + n,]$V2
              )
            )
          }
        }
        
        # my_igd <-
        #   rbind(my_igd,
        #         cbind(
        #           igd = igd(PF, Yref),
        #           iter = my.iter,
        #           fun = fun,
        #           Strategy = paste0(strategy[stg_idx], "_", lambda)
        #         ))
        # calc HV
        colnames(PF) <- colnames(ref1)
        
        
        PF <- scaling_Y(PF, ref1)
        hv <- dominated_hypervolume(t(PF), ref = ref.point)
        
        my_hv <-
          rbind(my_hv,
                cbind(
                  hv = hv,
                  iter = nfe,
                  fun = fun,
                  Strategy = strategy[stg_idx]
                ))
        # if (checkpoints[ck_idx] == checkpoints[length(checkpoints)])
        #   break
        
        ck_idx <- ck_idx + 1
      }
      
      # nfe <- 0
      total_hv <- rbind(total_hv, my_hv)
      # total_igd <- rbind(total_igd, my_igd)
    }
    
    
    stg_idx <- stg_idx + 1
    
  }
  
  # fun_hv <-
  #   rbind(fun_hv, total_hv)
  fun_hv <- total_hv
  
  # fun_igd <-
  #   rbind(fun_igd, total_igd)
  
  # fevals <- seq(to = 100000, by = 2500)
  # fevals <- fevals[1:10]
  # fun_hv$iter <- fevals
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
  
  
  filename = paste0("~/tec/hv_evolution/", fun , "hv_evolution.png")
  ggsave(
    filename = filename,
    dpi = 300,
    width = 12,
    height = 12
  )
  
}
fun_hv$hv <- as.numeric(as.character(fun_hv$hv))
# fun_igd$igd <- as.numeric(as.character(fun_igd$igd))


write_feather(fun_hv, "~/tec/hv_evolution/fun_hv")
# write_feather(fun_igd, "fun_igd")
