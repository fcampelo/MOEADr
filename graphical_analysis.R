library(feather)
library(MOEADr)
library(withr)
library(stringr)
library(emoa)
library(ggplot2)




calc_hv <- function(data, variation, fun, repetitions, max.val, min.val, ref.points = c(1,1), epsilon = 1e-50) {
  moead.data <- list()
  data <- (sweep(data, 2, min.val))/ ((max.val - min.val) + epsilon)
  non.d <- find_nondominated_points(data)
  if (sum(non.d) > 0){
  moead.hv <-
    dominated_hypervolume(points = t(data[non.d,]),
                                ref = ref.points)
  }
  else moead.hv <- 0
  temp <- data.frame(moead.hv, fun, paste0(variation), repetitions-1)
  
  colnames(temp) <-
    c("HV",
      "fun",
      "algorithm", "rep")
  temp
}

create_graphs <-
  function(data,
           fun.names,
           n.obj) {
    for (fun in fun.names) {
      my.data <- data[data$fun == fun, ]
      # my.data$algorithm <- factor(my.data$algorithm)
      pathname <- paste0("../HV-archive2/", fun, ".png")
      p <- ggplot(my.data, aes(algorithm, HV)) +
        geom_violin(aes(fill = algorithm), scale = "count") #+ ylim(0, 1) +
        # geom_violin(aes(fill = algorithm)) +
        ggtitle(fun) 
      p <-
        p + geom_boxplot(width = 0.4, alpha = 0.65) + theme(axis.text = element_text(size =
                                                                                       14),
                                                            axis.title =
                                                              element_text(size = 16, face = "bold")) + theme(plot.title = element_text(
                                                                color = "blue",
                                                                size = 24,
                                                                face = "bold"
                                                              )) + geom_jitter(height = 0, width = 0.1)
      
      ggsave(filename = pathname, device = "png")
    }
  }

create_graphs_log <-
  function(data,
           fun.names,
           n.obj) {
    data$HV <- log(data$HV)
    for (fun in fun.names) {
      my.data <- data[data$fun == fun,]
      pathname <- paste0("../HV-archive2/",fun, "_log.png")
      p <- ggplot(my.data, aes(algorithm, HV)) + 
        geom_violin(aes(fill = factor(algorithm))) 
      p + geom_boxplot(width=0.4, alpha = 0.65)
      ggsave(filename = pathname, device = "png")
    }
  }


