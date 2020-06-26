# library("dplyr")
# library(emoa)
# library(ecr)
# library(eaf)
library(feather)
# library(ggplot2)
# require(gridExtra)
#library(gganimate)
#library(hrbrthemes)
#library(ks)
#require(kohonen)
# library(MOEADr)
# library(mco)
# library(MASS)
#library(pracma)
# library(plyr) # used
# library(plotly)
# require(RColorBrewer)
# library(stringr)
# library(smoof)
# library(tidyverse) # used
#library(viridis)
# library(withr)





savePlotData <- function (moea, name, j,wd = '~/dataExp/') {
  if (is.null(moea$Archive)) {
    write_feather(data.frame(moea$X), paste0(wd, name, j, '_X'))
    write_feather(data.frame(moea$Y), paste0(wd, name, j, '_Y'))
  }
  else{
    write_feather(data.frame(moea$Archive$X),
                  paste0(wd, name, j, '_X'))
    write_feather(data.frame(moea$Archive$Y),
                  paste0(wd, name, j, '_Y'))
  }
  
  
  # temp <- moea$plot.paretofront[-1,]
  # temp <- apply(temp, 2, unlist)
  # temp <- as.data.frame(temp)
  # write_feather(temp, paste0(wd, name, j, '_plot.paretofront'))
  # 
  # temp <- as.data.frame(moea$plot.resources[-1,])
  # temp <- apply(temp, 2, unlist)
  # temp <- as.data.frame(temp)
  # write_feather(temp, paste0(wd, name, j, '_plot.resources'))
  
  temp <- as.data.frame(moea$plot.paretoset[-1, ])
  temp <- apply(temp, 2, unlist)
  temp <- as.data.frame(temp)
  write_feather(temp, paste0('~/dataExp/', name, j, '_plot.paretoset'))
  
  # temp <- as.data.frame(moea$W)
  # temp <- apply(temp, 2, unlist)
  # temp <- as.data.frame(temp)
  # write_feather(temp, paste0(wd, name, j, '_W'))
  # 
  # temp <- as.data.frame(moea$usage)
  # temp <- apply(temp, 2, unlist)
  # temp <- as.data.frame(temp)
  # write_feather(temp, paste0(wd, name, j, '_usage'))
  
  temp <- as.data.frame(moea$nfe)
  temp <- apply(temp, 2, unlist)
  temp <- as.data.frame(temp)
  write_feather(temp, paste0(wd, name, j, '_nfe'))
  
  temp <- as.data.frame(moea$n.iter)
  temp <- apply(temp, 2, unlist)
  temp <- as.data.frame(temp)
  write_feather(temp, paste0(wd, name, j, '_iter'))
  
  # temp <- as.data.frame(moea$time)
  # temp <- apply(temp, 2, unlist)
  # temp <- as.data.frame(temp)
  # write_feather(temp, paste0(wd, name, j, '_time'))
  
}

loadPlotData <- function (name, j,wd) {
  X <- read_feather(paste0(wd, name, j, '_X'))
  Y <- read_feather(paste0(wd, name, j, '_Y'))
  plot.paretofront <-
    read_feather(paste0(wd, name, j, '_plot.paretofront'))
  # plot.resources <-
  #   read_feather(paste0(wd, name, j, '_plot.resources'))
  iter <- read_feather(paste0(wd, name, j, '_iter'))
  # usage <- read_feather(paste0(wd, name, j, '_usage'))
  # time <- read_feather(paste0(wd, name, j, '_time'))
  # W <- read_feather(paste0(wd, name, j, '_W'))
  nfe <- read_feather(paste0(wd, name, j, '_nfe'))
  
  out <- list(
    X           = X,
    Y           = Y,
    # W           = W,
    nfe         = nfe,
    n.iter      = iter,
    # time        = time,
    # usage        = usage,
    #Vmatrix = Vmatrix,
    plot.paretofront = plot.paretofront,
    
    # plot.resources = plot.resources,
    moead.norm = FALSE
  )
  return(out)
  
}

# plot_eaf_eafdiff <-
#   function(data1, data2, n, name1, name2, fun, wd = "~/Desktop/") {
#     out <- savefile_create_fixs(name1)
#     prefix1 <- out$prefix1
#     posfix1 <- out$posfix1
#     out <- savefile_create_fixs(name2)
#     prefix2 <- out$prefix1
#     posfix2 <- out$posfix1
#     
#     if (n == 3) {
#       Y.pca.1 <-
#         prcomp(data1[, 1:n], center = TRUE, scale. = TRUE)
#       Y.pca.2 <-
#         prcomp(data2[, 1:n], center = TRUE, scale. = TRUE)
#       Fixed_0.05_Y <- as.data.frame(Y.pca.1$x[, 1:2])
#       Fixed_0.10_Y <- as.data.frame(Y.pca.2$x[, 1:2])
#       Fixed_0.05_Y$set <- data1$set
#       Fixed_0.10_Y$set <- data2$set
#     }
#     if (n == 2) {
#       Fixed_0.05_Y <- data1
#       Fixed_0.10_Y <- data2
#     }
#     png(
#       filename = paste0(
#         wd,
#         fun,
#         "eafplot_point",
#         prefix1,
#         posfix1,
#         "_",
#         prefix2,
#         posfix2,
#         ".png"
#       ),
#       width = 960
#     )
#     # png(filename = "~/Desktop/eafplot_point.png", width = 960)
#     eafplot(
#       list(Fixed_0.05 = Fixed_0.05_Y, Fixed_0.10 = Fixed_0.10_Y),
#       type = "point",
#       main = paste0(name1, "and", name2, " - PF Distribution"),
#       legend.pos = "bottomleft"
#     )
#     dev.off()
#     png(
#       filename = paste0(
#         wd,
#         fun,
#         "eafplot_area",
#         prefix1,
#         posfix1,
#         "_",
#         prefix2,
#         posfix2,
#         ".png"
#       ),
#       width = 960
#     )
#     # png(filename = "~/Desktop/eafplot_area.png", width = 960)
#     eafplot(
#       list(Fixed_0.05 = Fixed_0.05_Y, Fixed_0.10 = Fixed_0.10_Y),
#       type = "area",
#       main = paste0(name1, "and", name2, " - Area Covered"),
#       legend.pos = "bottomleft"
#     )
#     dev.off()
#     png(
#       filename = paste0(
#         wd,
#         fun,
#         "eafdiffplot_area",
#         prefix1,
#         posfix1,
#         "_",
#         prefix2,
#         posfix2,
#         ".png"
#       ),
#       width = 960
#     )
#     eafdiffplot(
#       data.left = Fixed_0.05_Y,
#       data.right = Fixed_0.10_Y,
#       type = "area",
#       title.left = paste0(name1, "and", name2, "Difference - Points Covered"),
#       title.right = paste0(name2, "and", name1, "Difference - Points Covered")
#     )
#     dev.off()
#     png(
#       filename = paste0(
#         wd,
#         fun,
#         "eafdiffplot_point",
#         prefix1,
#         posfix1,
#         "_",
#         prefix2,
#         posfix2,
#         ".png"
#       ),
#       width = 960
#     )
#     # png(filename = "~/Desktop/eafdiffplot_point.png", width = 960)
#     eafdiffplot(
#       data.left = Fixed_0.05_Y,
#       data.right = Fixed_0.10_Y,
#       type = "point",
#       title.left = paste0(name1, "and", name2, "Difference - Points Covered"),
#       title.right = paste0(name2, "and", name1, "Difference - Points Covered")
#     )
#     dev.off()
#   }
# 
