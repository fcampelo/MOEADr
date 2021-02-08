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
repetitions <- 3

checkpoints <- (0:10) * 10000
checkpoints[1] <- 1
dimensions <- 40
n.obj <- 2
pop.size <- 250

fun.names1 <- list()

for (i in 55:55) {
  fun.names1[[length(fun.names1) + 1]] = paste0("BIBBOB", i)
}

# for (i in 1:10) {
#   fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
# }
#
#
# for (i in 1:7) {
#   fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
# }
#
#
# for (i in 1:4) {
#   fun.names1[[length(fun.names1) + 1]] = paste0("inv_DTLZ", i)
# }



source("~/MOEADr/R/summary_moeadps.R")
source("~/MOEADr/R/utils.R")
# source("~/MOEADr/R/loadPlotData.R")
#
strategy <-
  c("NSGA-III-RA",
    "NSGA-III",
    "NSGA-II-RA",
    "NSGA-II",
    "MOEA/D-PS",
    "MOEA/D")

names <- c("nsga_3_ra",
           "nsga_3",
           "nsga_2_ra",
           "nsga_2",
           "moead_ra",
           "moead")



fun_hv <- data.frame()
results <- data.frame()

for (fun in fun.names1) {
  ref1 <- data.frame()
  print(fun)
  
  benchmark <- strsplit(fun, "[0-9]")[[1]][1]
  number <- strsplit(fun, "[A-Z]")[[1]][3]
  if (is.na(number))
    number <- -1
  if (benchmark == "DTLZ") {
    ref.point <- c(1, 1)
  }
  if (benchmark == "UF") {
    if (as.numeric(number) == 8 ||
        as.numeric(number) == 9 || as.numeric(number) == 10) {
      ref.point <- c(1, 1, 1)
      n.obj <- 3
    }
    else{
      ref.point <- c(1, 1)
    }
  }
  if (benchmark == "BIBBOB") {
    ref.point <- c(1, 1)
  }
  
  
  
  
  for (j in 1:repetitions) {
    for (k in 0:3990) {
      if (k <= 399) {
        moead <-
          read_feather(paste0("~/tec/", fun, "/moead_iter_", j, "/iter_", k))
        moead$Y <- moead[, (dimensions + 1):(dimensions + n.obj)]
        colnames(moead$Y) <- c("f1", "f2")
        
        nsga3 <-
          read_feather(paste0("~/tec/", fun, "/nsga_3_iter_", j, "/iter_", k))
        nsga3$Y <- nsga3[, (dimensions + 1):(dimensions + n.obj)]
        colnames(nsga3$Y) <- c("f1", "f2")
        
        nsga2 <-
          read_feather(paste0("~/tec/", fun, "/nsga_2_iter_", j, "/iter_", k))
        nsga2$Y <- nsga2[, (dimensions + 1):(dimensions + n.obj)]
        colnames(nsga2$Y) <- c("f1", "f2")
      }
      
      if (k <= 3837) {
        nsga3_ra <-
          read_feather(paste0("~/tec/", fun, "/nsga_3_ra_iter_", j, "/iter_", k))
        nsga3_ra$Y <-
          nsga3_ra[, (dimensions + 1):(dimensions + n.obj)]
        colnames(nsga3_ra$Y) <- c("f1", "f2")
        
        nsga2_ra <-
          read_feather(paste0("~/tec/", fun, "/nsga_2_ra_iter_", j, "/iter_", k))
        nsga2_ra$Y <-
          nsga2_ra[, (dimensions + 1):(dimensions + n.obj)]
        colnames(nsga2_ra$Y) <- c("f1", "f2")
      }
      
      moead_ra <-
        read_feather(paste0("~/tec/", fun, "/moead_ra_iter_", j, "/iter_", k))
      moead_ra$Y <-
        moead_ra[, (dimensions + 1):(dimensions + n.obj)]
      colnames(moead_ra$Y) <- c("f1", "f2")
      
      if (k > 3837) {
        ref1 <-
          rbind(ref1,
                moead_ra$Y)
      }
      else if (k > 399) {
        ref1 <-
          rbind(ref1,
                moead_ra$Y,
                nsga3_ra$Y,
                nsga2_ra$Y)
      }
      else {
        ref1 <-
          rbind(ref1,
                moead_ra$Y,
                moead$Y,
                nsga3_ra$Y,
                nsga3$Y,
                nsga2_ra$Y,
                nsga2$Y)
      }
    }
  }
  
  
  total_hv <- data.frame()
  
  my_hv <- data.frame()
  stg_idx <- 1
  for (name in names) {
    print(name)
    print(strategy[stg_idx])
    for (my_rep in 1:repetitions) {
      ck_idx <- 1
      my.PF <- data.frame()
      
      for (nfe.check in checkpoints) {
        if (nfe.check > 1) {
          iter <- ((nfe.check - 250) / 250) - 1
          
          if (name == "nsga_3_ra" || name == "nsga_2_ra") {
            my.nfe <- nfe.check - 250
            my.nfe <- floor(my.nfe / 26)
            iter <- my.nfe + 1
          }
          else if (name == "moead_ra") {
            my.nfe <- nfe.check - 250
            my.nfe <- floor(my.nfe / 25)
            iter <- my.nfe #+ 1
          }
          
          if (name == "nsga_3_ra" ||
              name == "nsga_2_ra" || name == "moead_ra") {
            for (idx in 1:5) {
              moea <-
                read_feather(paste0(
                  "~/tec/",
                  fun,
                  "/",
                  name,
                  "_iter_",
                  my_rep,
                  "/iter_",
                  iter - (idx - 1)
                ))
              moea$Y <-
                moea[, (dimensions + 1):(dimensions + n.obj)]
              colnames(moea$Y) <- c("f1", "f2")
              PF <- rbind(PF, moea$Y)
            }
          }
          else {
            moea <-
              read_feather(paste0(
                "~/tec/",
                fun,
                "/",
                name,
                "_iter_",
                my_rep,
                "/iter_",
                iter
              ))
            moea$Y <-
              moea[, (dimensions + 1):(dimensions + n.obj)]
            colnames(moea$Y) <- c("f1", "f2")
            PF <- moea$Y
          }
        }
        else{
          PF <- data.frame()
          iter <- 0
          moea <-
            read_feather(paste0("~/tec/",
                                fun,
                                "/",
                                name,
                                "_iter_",
                                my_rep,
                                "/iter_0"))
          moea$Y <- moea[, (dimensions + 1):(dimensions + n.obj)]
          colnames(moea$Y) <- c("f1", "f2")
          PF <- moea$Y
        }
        
        PF <- scaling_Y(PF, ref1)
        hv <- dominated_hypervolume(t(PF), ref = ref.point)
        
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
}

fun_hv <-
  rbind(fun_hv, total_hv)

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

print(median)

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

fun_hv$hv <- as.numeric(as.character(fun_hv$hv))
# fun_igd$igd <- as.numeric(as.character(fun_igd$igd))


write_feather(fun_hv, "~/tec/hv_evolution/fun_hv")
# write_feather(fun_igd, "fun_igd")
