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

for (i in 1:7) {
  fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}
for (i in 1:7) {
  fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
}


for (fun in fun.names1){
  f.data <- jpnsec_results[jpnsec_results$fun == fun, ]  
  
  plot <- 
    ggplot(f.data, aes(
      x = name, y = hv, fill = name
    )) + geom_boxplot() +
    theme_minimal(base_size = 26) +
    labs(x = "Strategy", y = "HV", face = "bold") +
    scale_fill_discrete(guide = FALSE)
  
  print(plot + theme(legend.position = "none", axis.text = element_text(size =24)))
  
  
  filename = paste0("~/jsec_2020_50/boxplot/hv_", fun, ".eps")
  ggsave(
    filename = filename,
    dpi = 1200,
    width = 12,
    height = 12
  )
  
  plot <- 
    ggplot(f.data, aes(
      x = name, y = igd, fill = name
    )) + geom_boxplot() +
    theme_minimal(base_size = 26) +
    labs(x = "Strategy", y = "IGD", face = "bold") +
    scale_fill_discrete(guide = FALSE)
  
  print(plot + theme(legend.position = "none", axis.text = element_text(size =24)))
  filename = paste0("~/jsec_2020_50/boxplot/igd_", fun, ".eps")
  ggsave(
    filename = filename,
    dpi = 1200,
    width = 12,
    height = 12
  )
}
