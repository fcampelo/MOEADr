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

number.fun <- 1
repetitions <- 10

checkpoints <- (0:20) * 5000
# checkpoints <- (0:2) * 50000
checkpoints[1] <- 500


fun.names1 <- list()

for (i in 1:55) {
  fun.names1[[length(fun.names1) + 1]] = paste0(i)
}
#
for (i in 1:10) {
  fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
}


for (i in 1:4) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}
#
#
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
           "moead50")

lambda <- 50


fun_hv <- data.frame()
results <- data.frame()
stats0 <- data.frame()

for (fun in fun.names1) {
  ref1 <- data.frame()
  print(fun)
  ref.point <- c(1, 1)
  divs <- 50
  benchmark <- strsplit(fun, "[0-9]")[[1]][1]
  number <- as.numeric(strsplit(fun, "[A-Z]")[[1]][3])
  if (is.na(number))
    number <- -1
  if (benchmark == "UF") {
    if (number == 8 ||
        number == 9 || number == 10) {
      ref.point <- c(1, 1, 1)
      divs <- 53
    }
  }
  
  
  for (j in 1:repetitions) {
    moead50 <-
      loadPlotData(
        name = paste0(fun, "_moead50_", lambda, "_"),
        j = j,
        wd = "~/../../../Volumes/LACIE SHARE/biman/tec/"
      )
    moead50$n.iter <- as.integer(moead50$n.iter)
    tmp1 <- data.frame()
    for (i in 1:moead50$n.iter) {
      if (benchmark == "UF" && number >= 8) {
        tmp1 <- rbind(
          tmp1,
          cbind(
            moead50$plot.paretofront[moead50$plot.paretofront$stage == i, ]$V1,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == i, ]$V2,
            moead50$plot.paretofront[moead50$plot.paretofront$stage == i, ]$V3
          )
        )
      }
      else{
        tmp1 <- rbind(tmp1,
                      cbind(
                        moead50$plot.paretofront[moead50$plot.paretofront$stage == i, ]$V1,
                        moead50$plot.paretofront[moead50$plot.paretofront$stage == i, ]$V2
                      ))
      }
    }
    
    if (benchmark == "UF" && number >= 8) {
      tmp1 <- rbind(
        tmp1,
        cbind(
          moead50$plot.paretofront[moead50$plot.paretofront$stage == moead50$n.iter, ]$V1,
          moead50$plot.paretofront[moead50$plot.paretofront$stage == moead50$n.iter, ]$V2,
          moead50$plot.paretofront[moead50$plot.paretofront$stage == moead50$n.iter, ]$V3
        )
      )
    }
    else{
      tmp1 <- rbind(tmp1,
                    cbind(
                      moead50$plot.paretofront[moead50$plot.paretofront$stage == moead50$n.iter, ]$V1,
                      moead50$plot.paretofront[moead50$plot.paretofront$stage == moead50$n.iter, ]$V2
                    ))
    }
    
    moead500 <-
      loadPlotData(
        name = paste0(fun, "_moead500_", lambda, "_"),
        j = j,
        wd = "~/../../../Volumes/LACIE SHARE/biman/tec/"
      )
    moead500$n.iter <- as.integer(moead500$n.iter)
    tmp2 <- data.frame()
    for (i in 1:moead500$n.iter) {
      if (benchmark == "UF" && number >= 8) {
        tmp2 <- rbind(
          tmp2,
          cbind(
            moead500$plot.paretofront[moead500$plot.paretofront$stage == i, ]$V1,
            moead500$plot.paretofront[moead500$plot.paretofront$stage == i, ]$V2,
            moead500$plot.paretofront[moead500$plot.paretofront$stage == i, ]$V3
          )
        )
      }
      else{
        tmp2 <- rbind(
          tmp2,
          cbind(
            moead500$plot.paretofront[moead500$plot.paretofront$stage == i, ]$V1,
            moead500$plot.paretofront[moead500$plot.paretofront$stage == i, ]$V2
          )
        )
      }
    }
    if (benchmark == "UF" && number >= 8) {
      tmp2 <- rbind(
        tmp2,
        cbind(
          moead500$plot.paretofront[moead500$plot.paretofront$stage == moead500$n.iter, ]$V1,
          moead500$plot.paretofront[moead500$plot.paretofront$stage == moead500$n.iter, ]$V2,
          moead500$plot.paretofront[moead500$plot.paretofront$stage == moead500$n.iter, ]$V3
        )
      )
    }
    else{
      tmp2 <- rbind(
        tmp2,
        cbind(
          moead500$plot.paretofront[moead500$plot.paretofront$stage == moead500$n.iter, ]$V1,
          moead500$plot.paretofront[moead500$plot.paretofront$stage == moead500$n.iter, ]$V2
        )
      )
    }
    
    moead.ps.50 <-
      loadPlotData(
        name = paste0(fun, "_moead.ps.50_", lambda, "_"),
        j = j,
        wd = "~/../../../Volumes/LACIE SHARE/biman/tec/"
      )
    moead.ps.50$n.iter <- as.integer(moead.ps.50$n.iter)
    tmp3 <- data.frame()
    for (i in 1:moead.ps.50$n.iter) {
      if (benchmark == "UF" && number >= 8) {
        tmp3 <- rbind(
          tmp3,
          cbind(
            moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == i, ]$V1,
            moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == i, ]$V2,
            moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == i, ]$V3
          )
        )
      }
      else{
        tmp3 <- rbind(
          tmp3,
          cbind(
            moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == i, ]$V1,
            moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == i, ]$V2
          )
        )
      }
    }
    if (benchmark == "UF" && number >= 8) {
      tmp3 <- rbind(
        tmp3,
        cbind(
          moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == moead.ps.50$n.iter, ]$V1,
          moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == moead.ps.50$n.iter, ]$V2,
          moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == moead.ps.50$n.iter, ]$V3
        )
      )
    }
    else{
      tmp3 <- rbind(
        tmp3,
        cbind(
          moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == moead.ps.50$n.iter, ]$V1,
          moead.ps.50$plot.paretofront[moead.ps.50$plot.paretofront$stage == moead.ps.50$n.iter, ]$V2
        )
      )
    }
    
    ref1 <-
      rbind(ref1,
            tmp1,
            tmp2,
            tmp3)
  }
  
  
  #############################################################################################################################
  #############################################################################################################################
  #############################################################################################################################
  
  
  total_hv <- data.frame()
  
  stg_idx <- 1
  for (name in names) {
    my_hv <- data.frame()
    print(strategy[stg_idx])
    for (my_rep in 1:repetitions) {
      ck_idx <- 1
      moea <-
        loadPlotData(
          name = paste0(fun, "_", name, "_", lambda, "_"),
          j = my_rep,
          wd = "~/../../../Volumes/LACIE SHARE/biman/tec/"
        )
      moea$n.iter <- as.integer(moea$n.iter)
      
      for (nfe.check in checkpoints) {
        PF <- data.frame()
        if (name == "moead50") {
          # moead 50
          iter <- floor(((nfe.check) / 50))
          if (iter <= 0)
            iter <- 1
          if(nfe.check == checkpoints[length(checkpoints)]) iter <- as.numeric(moea$n.iter)
          if (benchmark == "UF" && number >= 8) {
            for (x in 0:9) {
                tmp <- cbind(
                  moea$plot.paretofront[moea$plot.paretofront$stage == iter - x,]$V1,
                  moea$plot.paretofront[moea$plot.paretofront$stage == iter - x,]$V2,
                  moea$plot.paretofront[moea$plot.paretofront$stage == iter - x,]$V3
                )
              PF <- rbind(PF, tmp)
            }
          }
          else{
            for (x in 0:9) {
              tmp <-
                cbind(moea$plot.paretofront[moea$plot.paretofront$stage == iter - x,]$V1,
                      moea$plot.paretofront[moea$plot.paretofront$stage == iter - x,]$V2)
              PF <- rbind(PF, tmp)
            }
          }
        }
        
        else {
          if (name == "moead.ps.50") {
            # moead ps 50
            iter <- ceiling(((nfe.check - 500) / divs)) 
            if (iter <= 0)
              iter <- 1
          }
          
          else if (name == "moead500") {
            # moead 500
            iter <- floor(((nfe.check) / 500))
            if (iter <= 0)
              iter <- 1
          }
          if(nfe.check == checkpoints[length(checkpoints)]) iter <- as.numeric(moea$n.iter)
          if (benchmark == "UF" && number >= 8) {
            # for (x in 0:0) {
            tmp <- cbind(
              moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V1,
              moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V2,
              moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V3
            )
            PF <- rbind(PF, tmp)
            # }
          }
          else{
            # for (x in 0:0) {
            tmp <-
              cbind(moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V1,
                    moea$plot.paretofront[moea$plot.paretofront$stage == iter,]$V2)
            
            # }
            PF <- rbind(PF, tmp)
          }
        }
        
        colnames(PF) <- colnames(ref1)
        # cat("iter", iter, "\n")
        PF <- scaling_Y(PF, ref1)
        hv <- dominated_hypervolume(t(PF), ref = ref.point)
        
        
        my_hv <-
          rbind(my_hv,
                cbind(
                  hv = hv,
                  iter = checkpoints[ck_idx],
                  fun = fun,
                  Strategy = strategy[stg_idx]
                ))
        
        
        
        if (checkpoints[ck_idx] == checkpoints[length(checkpoints)]){
          
          nndom.idx <- ecr::nondominated(t(PF))
          nndom <- sum(nndom.idx)
          
          
          results <- rbind(results,
                           cbind(
                             hv = hv,
                             fun = fun,
                             name = strategy[stg_idx],
                             nndom = nndom
                           ))
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
  
  
  # filename = paste0("~/tec/hv_evolution/", fun , "hv_evolution.png")
  # ggsave(
  #   filename = filename,
  #   dpi = 300,
  #   width = 12,
  #   height = 12
  # )
  
  
}

write_feather(results, "~/tec/ecj_stats")



stats0$hv <- as.numeric(levels(stats0$hv))[stats0$hv]

write_feather(stats0, "~/tec/ecj_stats")

