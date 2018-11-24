library(feather)
library(MOEADr)
library(withr)
library(stringr)


setwd("~/MOEADr/R/")





calc_hv <- function(variation, fun, repetitions, ref.points = c(1,1), mod = FALSE){
  scaling <- list()
  moead.data <- list()
  scaling$name <- "simple"
  
  for (i in 1:repetitions){
    if (isTRUE(mod)){
      my.file.n <- paste0("../../",variation,"_mod/",variation, with_options(c(scipen = 999),
                                                                             str_pad(i-1, 3, pad = "0")), "_", fun)  
    }
    else{
      my.file.n <- paste0("../../",variation,"/",variation, with_options(c(scipen = 999),
                                                                         str_pad(i-1, 3, pad = "0")), "_", fun)
    }
    
    moead <- as.matrix(read_feather(my.file.n))
    print(my.file.n)
    print(moead)
    moead <-scale_objectives(moead, moead, scaling = scaling)$Y
    moead.non.d <- find_nondominated_points(moead)
    moead.hv <-
      emoa::dominated_hypervolume(points = t(moead[moead.non.d, ]),
                                  ref = ref.points)
    
    moead.data <-
      rbind(moead.data, moead.hv)
  }
  moead.data <- as.data.frame(unlist(moead.data))
  temp <- data.frame(moead.data, fun, paste0(variation,".",mod), (1:repetitions)-1)
  
  colnames(temp) <-
    c("HV",
      "fun",
      "variation.name")
  temp
}

create_graphs <-
  function(data,
           fun.names,
           n.obj) {
    aux <- data[data$base.algorithm == "moead.de",]
    for (fun in fun.names) {
      # aux1 <- aux[aux$fun == fun, ]
      pathname <- paste0("../HV-archive2/moead.de_", fun, ".png")
      png(pathname,
          width = 1000,
          height = 600)
      par(cex.axis = 1.5)
      boxplot(
        my.data[my.data$fun == fun,]$HV ~ my.data[my.data$fun == fun,]$variation.name,
        col = c("blue", "gray", "orange", "green", "brown"),
        las = 1
      )
      dev.off()
    }
  }




fun.names <- list()
for (i in 1:1) {
  fun.names[[length(fun.names) + 1]] = paste0("BiObjBBOB", i)
}
n.obj <- 2
benchmark = "BiObjBBOB"
repetitions <- 8
ref.points<- rep(1+1/149,2)


my.data0 <- calc_hv("rad", fun, repetitions, ref.points)
my.data1 <- calc_hv("rad", fun, repetitions, ref.points, mod = TRUE)
my.data2 <- calc_hv("de", fun, repetitions, ref.points)
my.data5 <- calc_hv("de", fun, repetitions, ref.points, mod = TRUE)
my.data3 <- calc_hv("gra", fun, repetitions, ref.points)
my.data4 <- calc_hv("dra", fun, repetitions, ref.points)
my.data6 <- calc_hv("nsga.2", fun, repetitions, ref.points)

my.data <- rbind(my.data0, my.data1, my.data3, my.data4, my.data5)


create_graphs(my.data, fun.names)
