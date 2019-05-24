setwd("~/")
library(feather)
library(ggplot2)
library(plyr)

create_graphs <-
  function(my.data) {
    # my.data$algorithm <- factor(my.data$algorithm,levels = c("MOEA/D-DE",  "MOEA/D-MRDL",  "MOEA/D-DRA"))
    my.data$algorithm <-
      revalue(
        my.data$algorithm,
        c(
          "MRDL" = "MOEA/D-RAD",
          "None" = "MOEA/D-DE",
          "DRA" = "MOEA/D-DRA"
        )
      )
    pathname <- paste0("../files/", my.data$fun, "_HV.eps")
    p <-
      ggplot(my.data, aes(algorithm, HV)) + geom_boxplot(aes(fill = algorithm))
    p <-
      p + theme(axis.text = element_text(size =
                                           13),
                axis.title =
                  element_text(size = 18)) + theme(plot.title = element_text(
                    color = "blue",
                    size = 18,
                    face = "bold"
                  )) + geom_jitter(height = 0, width = 0.1)
    p <- p + labs(title = paste0("HV - ", my.data$fun),
                  x = "Priority Function")
    p <- p + theme(legend.position = "none")
    print(p)
    ggsave(filename = pathname,
           device = "eps",
           dpi = 300)
  }

bbob.non <- data.frame()
bbob.non.sd <- data.frame()
nondon = list()
sd.nondon = list()


BiObjBBOB1 <- read_feather("MOEADr/forboxplot_BiObjBBOB1")
algorithm1 <- unique(BiObjBBOB1$algorithm)
BiObjBBOB1 <- BiObjBBOB1[BiObjBBOB1$algorithm != "GRA",]
aggregate(BiObjBBOB1$HV, median, by = list(BiObjBBOB1$algorithm))
temp = aggregate(BiObjBBOB1$HV, sd, by = list(BiObjBBOB1$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB1$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB1[BiObjBBOB1$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB1[BiObjBBOB1$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB1[BiObjBBOB1$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB1[BiObjBBOB1$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB2 <- read_feather("MOEADr/forboxplot_BiObjBBOB2")
BiObjBBOB2 <- BiObjBBOB2[BiObjBBOB2$algorithm != "GRA",]
aggregate(BiObjBBOB2$HV, median, by = list(BiObjBBOB2$algorithm))
temp = aggregate(BiObjBBOB2$HV, sd, by = list(BiObjBBOB2$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB2$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB2[BiObjBBOB2$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB2[BiObjBBOB2$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB2[BiObjBBOB2$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB2[BiObjBBOB2$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB3 <- read_feather("MOEADr/forboxplot_BiObjBBOB3")
BiObjBBOB3 <- BiObjBBOB3[BiObjBBOB3$algorithm != "GRA",]
aggregate(BiObjBBOB3$HV, median, by = list(BiObjBBOB3$algorithm))
temp = aggregate(BiObjBBOB3$HV, sd, by = list(BiObjBBOB3$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB3$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB3[BiObjBBOB3$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB3[BiObjBBOB3$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB3[BiObjBBOB3$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB3[BiObjBBOB3$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB4 <- read_feather("MOEADr/forboxplot_BiObjBBOB4")
BiObjBBOB4 <- BiObjBBOB4[BiObjBBOB4$algorithm != "GRA",]
aggregate(BiObjBBOB4$HV, median, by = list(BiObjBBOB4$algorithm))
temp = aggregate(BiObjBBOB4$HV, sd, by = list(BiObjBBOB4$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB4$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB4[BiObjBBOB4$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB4[BiObjBBOB4$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB4[BiObjBBOB4$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB4[BiObjBBOB4$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB5 <- read_feather("MOEADr/forboxplot_BiObjBBOB5")
BiObjBBOB5 <- BiObjBBOB5[BiObjBBOB5$algorithm != "GRA",]
aggregate(BiObjBBOB5$HV, median, by = list(BiObjBBOB5$algorithm))
temp = aggregate(BiObjBBOB5$HV, sd, by = list(BiObjBBOB5$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB5$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB5[BiObjBBOB5$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB5[BiObjBBOB5$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB5[BiObjBBOB5$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB5[BiObjBBOB5$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB6 <- read_feather("MOEADr/forboxplot_BiObjBBOB6")
BiObjBBOB6 <- BiObjBBOB6[BiObjBBOB6$algorithm != "GRA",]
aggregate(BiObjBBOB6$HV, median, by = list(BiObjBBOB6$algorithm))
temp = aggregate(BiObjBBOB6$HV, sd, by = list(BiObjBBOB6$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB6$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB6[BiObjBBOB6$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB6[BiObjBBOB6$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB6[BiObjBBOB6$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB6[BiObjBBOB6$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB7 <- read_feather("MOEADr/forboxplot_BiObjBBOB7")
BiObjBBOB7 <- BiObjBBOB7[BiObjBBOB7$algorithm != "GRA",]
aggregate(BiObjBBOB7$HV, median, by = list(BiObjBBOB7$algorithm))
temp = aggregate(BiObjBBOB7$HV, sd, by = list(BiObjBBOB7$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB7$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB7[BiObjBBOB7$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB7[BiObjBBOB7$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB7[BiObjBBOB7$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB7[BiObjBBOB7$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB8 <- read_feather("MOEADr/forboxplot_BiObjBBOB8")
BiObjBBOB8 <- BiObjBBOB8[BiObjBBOB8$algorithm != "GRA",]
aggregate(BiObjBBOB8$HV, median, by = list(BiObjBBOB8$algorithm))
temp = aggregate(BiObjBBOB8$HV, sd, by = list(BiObjBBOB8$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB8$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB8[BiObjBBOB8$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB8[BiObjBBOB8$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB8[BiObjBBOB8$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB8[BiObjBBOB8$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB9 <- read_feather("MOEADr/forboxplot_BiObjBBOB9")
BiObjBBOB9 <- BiObjBBOB9[BiObjBBOB9$algorithm != "GRA",]
aggregate(BiObjBBOB9$HV, median, by = list(BiObjBBOB9$algorithm))
temp = aggregate(BiObjBBOB9$HV, sd, by = list(BiObjBBOB9$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB9$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB9[BiObjBBOB9$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB9[BiObjBBOB9$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB9[BiObjBBOB9$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB9[BiObjBBOB9$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB10 <- read_feather("MOEADr/forboxplot_BiObjBBOB10")
BiObjBBOB10 <- BiObjBBOB10[BiObjBBOB10$algorithm != "GRA",]
aggregate(BiObjBBOB10$HV, median, by = list(BiObjBBOB10$algorithm))
temp = aggregate(BiObjBBOB10$HV, sd, by = list(BiObjBBOB10$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB10$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB10[BiObjBBOB10$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB10[BiObjBBOB10$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB10[BiObjBBOB10$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB10[BiObjBBOB10$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB11 <- read_feather("MOEADr/forboxplot_BiObjBBOB11")
BiObjBBOB11 <- BiObjBBOB11[BiObjBBOB11$algorithm != "GRA",]

aggregate(BiObjBBOB11$HV, median, by = list(BiObjBBOB11$algorithm))
temp = aggregate(BiObjBBOB11$HV, sd, by = list(BiObjBBOB11$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB11$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB11[BiObjBBOB11$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB11[BiObjBBOB11$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB11[BiObjBBOB11$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB11[BiObjBBOB11$algorithm == algorithm,]$nondominated /
               150), 2)
}

BiObjBBOB12 <- read_feather("MOEADr/forboxplot_BiObjBBOB12")
BiObjBBOB12 <- BiObjBBOB12[BiObjBBOB12$algorithm != "GRA",]
aggregate(BiObjBBOB12$HV, median, by = list(BiObjBBOB12$algorithm))
temp = aggregate(BiObjBBOB12$HV, sd, by = list(BiObjBBOB12$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB12$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB12[BiObjBBOB12$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB12[BiObjBBOB12$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB12[BiObjBBOB12$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB12[BiObjBBOB12$algorithm == algorithm,]$nondominated /
               150), 2)
}
BiObjBBOB13 <- read_feather("MOEADr/forboxplot_BiObjBBOB13")
BiObjBBOB13 <- BiObjBBOB13[BiObjBBOB13$algorithm != "GRA",]
aggregate(BiObjBBOB13$HV, median, by = list(BiObjBBOB13$algorithm))
temp = aggregate(BiObjBBOB13$HV, sd, by = list(BiObjBBOB13$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB13$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB13[BiObjBBOB13$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB13[BiObjBBOB13$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB13[BiObjBBOB13$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB13[BiObjBBOB13$algorithm == algorithm,]$nondominated /
               150), 2)
}
BiObjBBOB14 <- read_feather("MOEADr/forboxplot_BiObjBBOB14")
BiObjBBOB14 <- BiObjBBOB14[BiObjBBOB14$algorithm != "GRA",]
aggregate(BiObjBBOB14$HV, median, by = list(BiObjBBOB14$algorithm))
temp = aggregate(BiObjBBOB14$HV, sd, by = list(BiObjBBOB14$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB14$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB14[BiObjBBOB14$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB14[BiObjBBOB14$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB14[BiObjBBOB14$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB14[BiObjBBOB14$algorithm == algorithm,]$nondominated /
               150), 2)
}
BiObjBBOB15 <- read_feather("MOEADr/forboxplot_BiObjBBOB15")
BiObjBBOB15 <- BiObjBBOB15[BiObjBBOB15$algorithm != "GRA",]
aggregate(BiObjBBOB15$HV, median, by = list(BiObjBBOB15$algorithm))
temp = aggregate(BiObjBBOB15$HV, sd, by = list(BiObjBBOB15$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB15$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB15[BiObjBBOB15$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB15[BiObjBBOB15$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB15[BiObjBBOB15$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB15[BiObjBBOB15$algorithm == algorithm,]$nondominated /
               150), 2)
}
BiObjBBOB16 <- read_feather("MOEADr/forboxplot_BiObjBBOB16")
BiObjBBOB16 <- BiObjBBOB16[BiObjBBOB16$algorithm != "GRA",]
aggregate(BiObjBBOB16$HV, median, by = list(BiObjBBOB16$algorithm))
temp = aggregate(BiObjBBOB16$HV, sd, by = list(BiObjBBOB16$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB16$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB16[BiObjBBOB16$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB16[BiObjBBOB16$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB16[BiObjBBOB16$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB16[BiObjBBOB16$algorithm == algorithm,]$nondominated /
               150), 2)
}
BiObjBBOB17 <- read_feather("MOEADr/forboxplot_BiObjBBOB17")
BiObjBBOB17 <- BiObjBBOB17[BiObjBBOB17$algorithm != "GRA",]
aggregate(BiObjBBOB17$HV, median, by = list(BiObjBBOB17$algorithm))
temp = aggregate(BiObjBBOB17$HV, sd, by = list(BiObjBBOB17$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB17$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB17[BiObjBBOB17$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB17[BiObjBBOB17$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB17[BiObjBBOB17$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB17[BiObjBBOB17$algorithm == algorithm,]$nondominated /
               150), 2)
}
BiObjBBOB18 <- read_feather("MOEADr/forboxplot_BiObjBBOB18")
BiObjBBOB18 <- BiObjBBOB18[BiObjBBOB18$algorithm != "GRA",]
aggregate(BiObjBBOB18$HV, median, by = list(BiObjBBOB18$algorithm))
temp = aggregate(BiObjBBOB18$HV, sd, by = list(BiObjBBOB18$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB18$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB18[BiObjBBOB18$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB18[BiObjBBOB18$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB18[BiObjBBOB18$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB18[BiObjBBOB18$algorithm == algorithm,]$nondominated /
               150), 2)
}
BiObjBBOB19 <- read_feather("MOEADr/forboxplot_BiObjBBOB19")
BiObjBBOB19 <- BiObjBBOB19[BiObjBBOB19$algorithm != "GRA",]
aggregate(BiObjBBOB19$HV, median, by = list(BiObjBBOB19$algorithm))
temp = aggregate(BiObjBBOB19$HV, sd, by = list(BiObjBBOB19$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB19$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB19[BiObjBBOB19$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB19[BiObjBBOB19$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB19[BiObjBBOB19$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB19[BiObjBBOB19$algorithm == algorithm,]$nondominated /
               150), 2)
}
BiObjBBOB20 <- read_feather("MOEADr/forboxplot_BiObjBBOB20")
BiObjBBOB20 <- BiObjBBOB20[BiObjBBOB20$algorithm != "GRA",]
aggregate(BiObjBBOB20$HV, median, by = list(BiObjBBOB20$algorithm))
temp = aggregate(BiObjBBOB20$HV, sd, by = list(BiObjBBOB20$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB20$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB20[BiObjBBOB20$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB20[BiObjBBOB20$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB20[BiObjBBOB20$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB20[BiObjBBOB20$algorithm == algorithm,]$nondominated /
               150), 2)
}
BiObjBBOB21 <- read_feather("MOEADr/forboxplot_BiObjBBOB21")
BiObjBBOB21 <- BiObjBBOB21[BiObjBBOB21$algorithm != "GRA",]
aggregate(BiObjBBOB21$HV, median, by = list(BiObjBBOB21$algorithm))
temp = aggregate(BiObjBBOB21$HV, sd, by = list(BiObjBBOB21$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB21$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB21[BiObjBBOB21$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB21[BiObjBBOB21$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB21[BiObjBBOB21$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB21[BiObjBBOB21$algorithm == algorithm,]$nondominated /
               150), 2)
}
BiObjBBOB22 <- read_feather("MOEADr/forboxplot_BiObjBBOB22")
BiObjBBOB22 <- BiObjBBOB22[BiObjBBOB22$algorithm != "GRA",]
aggregate(BiObjBBOB22$HV, median, by = list(BiObjBBOB22$algorithm))
temp = aggregate(BiObjBBOB22$HV, sd, by = list(BiObjBBOB22$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB22$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB22[BiObjBBOB22$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB22[BiObjBBOB22$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB22[BiObjBBOB22$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB22[BiObjBBOB22$algorithm == algorithm,]$nondominated /
               150), 2)
}
BiObjBBOB23 <- read_feather("MOEADr/forboxplot_BiObjBBOB23")
BiObjBBOB23 <- BiObjBBOB23[BiObjBBOB23$algorithm != "GRA",]
aggregate(BiObjBBOB23$HV, median, by = list(BiObjBBOB23$algorithm))
temp = aggregate(BiObjBBOB23$HV, sd, by = list(BiObjBBOB23$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB23$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB23[BiObjBBOB23$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB23[BiObjBBOB23$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB23[BiObjBBOB23$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB23[BiObjBBOB23$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB24 <- read_feather("MOEADr/forboxplot_BiObjBBOB24")
BiObjBBOB24 <- BiObjBBOB24[BiObjBBOB24$algorithm != "GRA",]
aggregate(BiObjBBOB24$HV, median, by = list(BiObjBBOB24$algorithm))
temp = aggregate(BiObjBBOB24$HV, sd, by = list(BiObjBBOB24$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB24$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB24[BiObjBBOB24$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB24[BiObjBBOB24$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB24[BiObjBBOB24$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB24[BiObjBBOB24$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB25 <- read_feather("MOEADr/forboxplot_BiObjBBOB25")
BiObjBBOB25 <- BiObjBBOB25[BiObjBBOB25$algorithm != "GRA",]
aggregate(BiObjBBOB25$HV, median, by = list(BiObjBBOB25$algorithm))
temp = aggregate(BiObjBBOB25$HV, sd, by = list(BiObjBBOB25$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB25$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB25[BiObjBBOB25$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB25[BiObjBBOB25$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB25[BiObjBBOB25$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB25[BiObjBBOB25$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB26 <- read_feather("MOEADr/forboxplot_BiObjBBOB26")
BiObjBBOB26 <- BiObjBBOB26[BiObjBBOB26$algorithm != "GRA",]
aggregate(BiObjBBOB26$HV, median, by = list(BiObjBBOB26$algorithm))
temp = aggregate(BiObjBBOB26$HV, sd, by = list(BiObjBBOB26$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB26$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB26[BiObjBBOB26$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB26[BiObjBBOB26$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB26[BiObjBBOB26$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB26[BiObjBBOB26$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB27 <- read_feather("MOEADr/forboxplot_BiObjBBOB27")
BiObjBBOB27 <- BiObjBBOB27[BiObjBBOB27$algorithm != "GRA",]
aggregate(BiObjBBOB27$HV, median, by = list(BiObjBBOB27$algorithm))
temp = aggregate(BiObjBBOB27$HV, sd, by = list(BiObjBBOB27$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB27$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB27[BiObjBBOB27$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB27[BiObjBBOB27$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB27[BiObjBBOB27$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB27[BiObjBBOB27$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB28 <- read_feather("MOEADr/forboxplot_BiObjBBOB28")
BiObjBBOB28 <- BiObjBBOB28[BiObjBBOB28$algorithm != "GRA",]
aggregate(BiObjBBOB28$HV, median, by = list(BiObjBBOB28$algorithm))
temp = aggregate(BiObjBBOB28$HV, sd, by = list(BiObjBBOB28$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB28$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB28[BiObjBBOB28$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB28[BiObjBBOB28$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB28[BiObjBBOB28$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB28[BiObjBBOB28$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB29 <- read_feather("MOEADr/forboxplot_BiObjBBOB29")
BiObjBBOB29 <- BiObjBBOB29[BiObjBBOB29$algorithm != "GRA",]
aggregate(BiObjBBOB29$HV, median, by = list(BiObjBBOB29$algorithm))
temp = aggregate(BiObjBBOB29$HV, sd, by = list(BiObjBBOB29$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB29$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB29[BiObjBBOB29$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB29[BiObjBBOB29$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB29[BiObjBBOB29$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB29[BiObjBBOB29$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB30 <- read_feather("MOEADr/forboxplot_BiObjBBOB30")
BiObjBBOB30 <- BiObjBBOB30[BiObjBBOB30$algorithm != "GRA",]
aggregate(BiObjBBOB30$HV, median, by = list(BiObjBBOB30$algorithm))
temp = aggregate(BiObjBBOB30$HV, sd, by = list(BiObjBBOB30$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB30$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB30[BiObjBBOB30$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB30[BiObjBBOB30$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB30[BiObjBBOB30$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB30[BiObjBBOB30$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()


BiObjBBOB31 <- read_feather("MOEADr/forboxplot_BiObjBBOB31")
BiObjBBOB31 <- BiObjBBOB31[BiObjBBOB31$algorithm != "GRA",]
aggregate(BiObjBBOB31$HV, median, by = list(BiObjBBOB31$algorithm))
temp = aggregate(BiObjBBOB31$HV, sd, by = list(BiObjBBOB31$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB31$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB31[BiObjBBOB31$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB31[BiObjBBOB31$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB31[BiObjBBOB31$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB31[BiObjBBOB31$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB32 <- read_feather("MOEADr/forboxplot_BiObjBBOB32")
BiObjBBOB32 <- BiObjBBOB32[BiObjBBOB32$algorithm != "GRA",]
aggregate(BiObjBBOB32$HV, median, by = list(BiObjBBOB32$algorithm))
temp = aggregate(BiObjBBOB32$HV, sd, by = list(BiObjBBOB32$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB32$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB32[BiObjBBOB32$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB32[BiObjBBOB32$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB32[BiObjBBOB32$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB32[BiObjBBOB32$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB33 <- read_feather("MOEADr/forboxplot_BiObjBBOB33")
BiObjBBOB33 <- BiObjBBOB33[BiObjBBOB33$algorithm != "GRA",]
aggregate(BiObjBBOB33$HV, median, by = list(BiObjBBOB33$algorithm))
temp = aggregate(BiObjBBOB33$HV, sd, by = list(BiObjBBOB33$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB33$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB33[BiObjBBOB33$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB33[BiObjBBOB33$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB33[BiObjBBOB33$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB33[BiObjBBOB33$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB34 <- read_feather("MOEADr/forboxplot_BiObjBBOB34")
BiObjBBOB34 <- BiObjBBOB34[BiObjBBOB34$algorithm != "GRA",]
aggregate(BiObjBBOB34$HV, median, by = list(BiObjBBOB34$algorithm))
temp = aggregate(BiObjBBOB34$HV, sd, by = list(BiObjBBOB34$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB34$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB34[BiObjBBOB34$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB34[BiObjBBOB34$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB34[BiObjBBOB34$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB34[BiObjBBOB34$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB35 <- read_feather("MOEADr/forboxplot_BiObjBBOB35")
BiObjBBOB35 <- BiObjBBOB35[BiObjBBOB35$algorithm != "GRA",]
aggregate(BiObjBBOB35$HV, median, by = list(BiObjBBOB35$algorithm))
temp = aggregate(BiObjBBOB35$HV, sd, by = list(BiObjBBOB35$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB35$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB35[BiObjBBOB35$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB35[BiObjBBOB35$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB35[BiObjBBOB35$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB35[BiObjBBOB35$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB36 <- read_feather("MOEADr/forboxplot_BiObjBBOB36")
BiObjBBOB36 <- BiObjBBOB36[BiObjBBOB36$algorithm != "GRA",]
aggregate(BiObjBBOB36$HV, median, by = list(BiObjBBOB36$algorithm))
temp = aggregate(BiObjBBOB36$HV, sd, by = list(BiObjBBOB36$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB36$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB36[BiObjBBOB36$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB36[BiObjBBOB36$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB36[BiObjBBOB36$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB36[BiObjBBOB36$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB37 <- read_feather("MOEADr/forboxplot_BiObjBBOB37")
BiObjBBOB37 <- BiObjBBOB37[BiObjBBOB37$algorithm != "GRA",]
aggregate(BiObjBBOB37$HV, median, by = list(BiObjBBOB37$algorithm))
temp = aggregate(BiObjBBOB37$HV, sd, by = list(BiObjBBOB37$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB37$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB37[BiObjBBOB37$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB37[BiObjBBOB37$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB37[BiObjBBOB37$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB37[BiObjBBOB37$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB38 <- read_feather("MOEADr/forboxplot_BiObjBBOB38")
BiObjBBOB38 <- BiObjBBOB38[BiObjBBOB38$algorithm != "GRA",]
aggregate(BiObjBBOB38$HV, median, by = list(BiObjBBOB38$algorithm))
temp = aggregate(BiObjBBOB38$HV, sd, by = list(BiObjBBOB38$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB38$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB38[BiObjBBOB38$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB38[BiObjBBOB38$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB38[BiObjBBOB38$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB38[BiObjBBOB38$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB39 <- read_feather("MOEADr/forboxplot_BiObjBBOB39")
BiObjBBOB39 <- BiObjBBOB39[BiObjBBOB39$algorithm != "GRA",]
aggregate(BiObjBBOB39$HV, median, by = list(BiObjBBOB39$algorithm))
temp = aggregate(BiObjBBOB39$HV, sd, by = list(BiObjBBOB39$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB39$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB39[BiObjBBOB39$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB39[BiObjBBOB39$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB39[BiObjBBOB39$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB39[BiObjBBOB39$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB40 <- read_feather("MOEADr/forboxplot_BiObjBBOB40")
BiObjBBOB40 <- BiObjBBOB40[BiObjBBOB40$algorithm != "GRA",]
aggregate(BiObjBBOB40$HV, median, by = list(BiObjBBOB40$algorithm))
temp = aggregate(BiObjBBOB40$HV, sd, by = list(BiObjBBOB40$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB40$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB40[BiObjBBOB40$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB40[BiObjBBOB40$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB40[BiObjBBOB40$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB40[BiObjBBOB40$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB41 <- read_feather("MOEADr/forboxplot_BiObjBBOB41")
BiObjBBOB41 <- BiObjBBOB41[BiObjBBOB41$algorithm != "GRA",]
aggregate(BiObjBBOB41$HV, median, by = list(BiObjBBOB41$algorithm))
temp = aggregate(BiObjBBOB41$HV, sd, by = list(BiObjBBOB41$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB41$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB41[BiObjBBOB41$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB41[BiObjBBOB41$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB41[BiObjBBOB41$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB41[BiObjBBOB41$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB42 <- read_feather("MOEADr/forboxplot_BiObjBBOB42")
BiObjBBOB42 <- BiObjBBOB42[BiObjBBOB42$algorithm != "GRA",]
aggregate(BiObjBBOB42$HV, median, by = list(BiObjBBOB42$algorithm))
temp = aggregate(BiObjBBOB42$HV, sd, by = list(BiObjBBOB42$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB42$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB42[BiObjBBOB42$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB42[BiObjBBOB42$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB42[BiObjBBOB42$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB42[BiObjBBOB42$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB43 <- read_feather("MOEADr/forboxplot_BiObjBBOB43")
BiObjBBOB43 <- BiObjBBOB43[BiObjBBOB43$algorithm != "GRA",]
aggregate(BiObjBBOB43$HV, median, by = list(BiObjBBOB43$algorithm))
temp = aggregate(BiObjBBOB43$HV, sd, by = list(BiObjBBOB43$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB43$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB43[BiObjBBOB43$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB43[BiObjBBOB43$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB43[BiObjBBOB43$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB43[BiObjBBOB43$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB44 <- read_feather("MOEADr/forboxplot_BiObjBBOB44")
BiObjBBOB44 <- BiObjBBOB44[BiObjBBOB44$algorithm != "GRA",]
aggregate(BiObjBBOB44$HV, median, by = list(BiObjBBOB44$algorithm))
temp = aggregate(BiObjBBOB44$HV, sd, by = list(BiObjBBOB44$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB44$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB44[BiObjBBOB44$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB44[BiObjBBOB44$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB44[BiObjBBOB44$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB44[BiObjBBOB44$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB45 <- read_feather("MOEADr/forboxplot_BiObjBBOB45")
BiObjBBOB45 <- BiObjBBOB45[BiObjBBOB45$algorithm != "GRA",]
aggregate(BiObjBBOB45$HV, median, by = list(BiObjBBOB45$algorithm))
temp = aggregate(BiObjBBOB45$HV, sd, by = list(BiObjBBOB45$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB45$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB45[BiObjBBOB45$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB45[BiObjBBOB45$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB45[BiObjBBOB45$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB45[BiObjBBOB45$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB46 <- read_feather("MOEADr/forboxplot_BiObjBBOB46")
BiObjBBOB46 <- BiObjBBOB46[BiObjBBOB46$algorithm != "GRA",]
aggregate(BiObjBBOB46$HV, median, by = list(BiObjBBOB46$algorithm))
temp = aggregate(BiObjBBOB46$HV, sd, by = list(BiObjBBOB46$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB46$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB46[BiObjBBOB46$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB46[BiObjBBOB46$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB46[BiObjBBOB46$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB46[BiObjBBOB46$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB47 <- read_feather("MOEADr/forboxplot_BiObjBBOB47")
BiObjBBOB47 <- BiObjBBOB47[BiObjBBOB47$algorithm != "GRA",]
aggregate(BiObjBBOB47$HV, median, by = list(BiObjBBOB47$algorithm))
temp = aggregate(BiObjBBOB47$HV, sd, by = list(BiObjBBOB47$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB47$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB47[BiObjBBOB47$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB47[BiObjBBOB47$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB47[BiObjBBOB47$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB47[BiObjBBOB47$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB48 <- read_feather("MOEADr/forboxplot_BiObjBBOB48")
BiObjBBOB48 <- BiObjBBOB48[BiObjBBOB48$algorithm != "GRA",]
aggregate(BiObjBBOB48$HV, median, by = list(BiObjBBOB48$algorithm))
temp = aggregate(BiObjBBOB48$HV, sd, by = list(BiObjBBOB48$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB48$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB48[BiObjBBOB48$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB48[BiObjBBOB48$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB48[BiObjBBOB48$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB48[BiObjBBOB48$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB49 <- read_feather("MOEADr/forboxplot_BiObjBBOB49")
BiObjBBOB49 <- BiObjBBOB49[BiObjBBOB49$algorithm != "GRA",]
aggregate(BiObjBBOB49$HV, median, by = list(BiObjBBOB49$algorithm))
temp = aggregate(BiObjBBOB49$HV, sd, by = list(BiObjBBOB49$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB49$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB49[BiObjBBOB49$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB49[BiObjBBOB49$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB49[BiObjBBOB49$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB49[BiObjBBOB49$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB50 <- read_feather("MOEADr/forboxplot_BiObjBBOB50")
BiObjBBOB50 <- BiObjBBOB50[BiObjBBOB50$algorithm != "GRA",]
aggregate(BiObjBBOB50$HV, median, by = list(BiObjBBOB50$algorithm))
temp = aggregate(BiObjBBOB50$HV, sd, by = list(BiObjBBOB50$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB50$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB50[BiObjBBOB50$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB50[BiObjBBOB50$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB50[BiObjBBOB50$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB50[BiObjBBOB50$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB51 <- read_feather("MOEADr/forboxplot_BiObjBBOB51")
BiObjBBOB51 <- BiObjBBOB51[BiObjBBOB51$algorithm != "GRA",]
aggregate(BiObjBBOB51$HV, median, by = list(BiObjBBOB51$algorithm))
temp = aggregate(BiObjBBOB51$HV, sd, by = list(BiObjBBOB51$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB51$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB51[BiObjBBOB51$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB51[BiObjBBOB51$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB51[BiObjBBOB51$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB51[BiObjBBOB51$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB52 <- read_feather("MOEADr/forboxplot_BiObjBBOB52")
BiObjBBOB52 <- BiObjBBOB52[BiObjBBOB52$algorithm != "GRA",]
aggregate(BiObjBBOB52$HV, median, by = list(BiObjBBOB52$algorithm))
temp = aggregate(BiObjBBOB52$HV, sd, by = list(BiObjBBOB52$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB52$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB52[BiObjBBOB52$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB52[BiObjBBOB52$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB52[BiObjBBOB52$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB52[BiObjBBOB52$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB53 <- read_feather("MOEADr/forboxplot_BiObjBBOB53")
BiObjBBOB53 <- BiObjBBOB53[BiObjBBOB53$algorithm != "GRA",]
aggregate(BiObjBBOB53$HV, median, by = list(BiObjBBOB53$algorithm))
temp = aggregate(BiObjBBOB53$HV, sd, by = list(BiObjBBOB53$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB53$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB53[BiObjBBOB53$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB53[BiObjBBOB53$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB53[BiObjBBOB53$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB53[BiObjBBOB53$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB54 <- read_feather("MOEADr/forboxplot_BiObjBBOB54")
BiObjBBOB54 <- BiObjBBOB54[BiObjBBOB54$algorithm != "GRA",]
aggregate(BiObjBBOB54$HV, median, by = list(BiObjBBOB54$algorithm))
temp = aggregate(BiObjBBOB54$HV, sd, by = list(BiObjBBOB54$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB54$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB54[BiObjBBOB54$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB54[BiObjBBOB54$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB54[BiObjBBOB54$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB54[BiObjBBOB54$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()

BiObjBBOB55 <- read_feather("MOEADr/forboxplot_BiObjBBOB55")
BiObjBBOB55 <- BiObjBBOB55[BiObjBBOB55$algorithm != "GRA",]
aggregate(BiObjBBOB55$HV, median, by = list(BiObjBBOB55$algorithm))
temp = aggregate(BiObjBBOB55$HV, sd, by = list(BiObjBBOB55$algorithm))
temp$x <- round(temp$x, 3)
temp
for (algorithm in unique(BiObjBBOB55$algorithm)) {
  print(algorithm)
  print(round(median(BiObjBBOB55[BiObjBBOB55$algorithm == algorithm,]$nondominated /
                       150), 4))
  print(round(sd(BiObjBBOB55[BiObjBBOB55$algorithm == algorithm,]$nondominated /
                   150), 2))
  nondon[length(nondon) + 1] <-
    round(median(BiObjBBOB55[BiObjBBOB55$algorithm == algorithm,]$nondominated /
                   150), 4)
  sd.nondon[length(nondon) + 1] <-
    round(sd(BiObjBBOB55[BiObjBBOB55$algorithm == algorithm,]$nondominated /
               150), 2)
}

bbob.non <- rbind(bbob.non, data.frame(unlist(nondon), algorithm1))
bbob.non.sd <-
  rbind(bbob.non.sd, data.frame(unlist(sd.nondon), algorithm1))
nondon <- list()
sd.nondon <- list()



setwd("~/MOEADr/")
# create_graphs(BiObjBBOB1)
# create_graphs(BiObjBBOB2)
# create_graphs(BiObjBBOB3)
# create_graphs(BiObjBBOB4)
# create_graphs(BiObjBBOB5)
# create_graphs(BiObjBBOB6)
# create_graphs(BiObjBBOB7)
# create_graphs(BiObjBBOB8)
# create_graphs(BiObjBBOB9)
# create_graphs(BiObjBBOB10)
# create_graphs(BiObjBBOB11)
# create_graphs(BiObjBBOB12)
# create_graphs(BiObjBBOB13)
# create_graphs(BiObjBBOB14)
# create_graphs(BiObjBBOB15)
# create_graphs(BiObjBBOB16)
# create_graphs(BiObjBBOB17)
# create_graphs(BiObjBBOB18)
# create_graphs(BiObjBBOB19)
# create_graphs(BiObjBBOB20)
# create_graphs(BiObjBBOB21)
# create_graphs(BiObjBBOB22)
# create_graphs(BiObjBBOB23)
# create_graphs(BiObjBBOB24)
# create_graphs(BiObjBBOB25)
# create_graphs(BiObjBBOB26)
# create_graphs(BiObjBBOB27)
# create_graphs(BiObjBBOB28)
# create_graphs(BiObjBBOB29)
# create_graphs(BiObjBBOB30)
# create_graphs(BiObjBBOB31)
# create_graphs(BiObjBBOB32)
# create_graphs(BiObjBBOB33)
# create_graphs(BiObjBBOB34)
# create_graphs(BiObjBBOB35)
# create_graphs(BiObjBBOB36)
# create_graphs(BiObjBBOB37)
# create_graphs(BiObjBBOB38)
# create_graphs(BiObjBBOB39)
# create_graphs(BiObjBBOB40)
# create_graphs(BiObjBBOB41)
# create_graphs(BiObjBBOB42)
# create_graphs(BiObjBBOB43)
# create_graphs(BiObjBBOB44)
# create_graphs(BiObjBBOB46)
# create_graphs(BiObjBBOB47)
# create_graphs(BiObjBBOB48)
# create_graphs(BiObjBBOB49)
# create_graphs(BiObjBBOB50)
# create_graphs(BiObjBBOB51)
# create_graphs(BiObjBBOB52)
# create_graphs(BiObjBBOB53)
# create_graphs(BiObjBBOB54)
# create_graphs(BiObjBBOB55)

BiObjBBOB <- cbind (
  BiObjBBOB1,
  BiObjBBOB2,
  BiObjBBOB3,
  BiObjBBOB4,
  BiObjBBOB5,
  BiObjBBOB6,
  BiObjBBOB7,
  BiObjBBOB8,
  BiObjBBOB9,
  BiObjBBOB10,
  BiObjBBOB11,
  BiObjBBOB12,
  BiObjBBOB13,
  BiObjBBOB14,
  BiObjBBOB15,
  BiObjBBOB16,
  BiObjBBOB17,
  BiObjBBOB18,
  BiObjBBOB19,
  BiObjBBOB20,
  BiObjBBOB21,
  BiObjBBOB22,
  BiObjBBOB24,
  BiObjBBOB23,
  BiObjBBOB25,
  BiObjBBOB26,
  BiObjBBOB27,
  BiObjBBOB28,
  BiObjBBOB29,
  BiObjBBOB30,
  BiObjBBOB31,
  BiObjBBOB32,
  BiObjBBOB33,
  BiObjBBOB34,
  BiObjBBOB35,
  BiObjBBOB36,
  BiObjBBOB37,
  BiObjBBOB38,
  BiObjBBOB39,
  BiObjBBOB40,
  BiObjBBOB41,
  BiObjBBOB42,
  BiObjBBOB43,
  BiObjBBOB44,
  BiObjBBOB46,
  BiObjBBOB47,
  BiObjBBOB48,
  BiObjBBOB49,
  BiObjBBOB50,
  BiObjBBOB51,
  BiObjBBOB52,
  BiObjBBOB53,
  BiObjBBOB54,
  BiObjBBOB55
)
setwd("~/")
print(pairwise.wilcox.test(BiObjBBOB$HV, BiObjBBOB55$algorithm, p.adjust.method = "hommel", paired = T))
print(pairwise.wilcox.test(BiObjBBOB$HV, BiObjBBOB55$algorithm, p.adjust.method = "hommel"))
# 
print("separable - separable (f_1, f_2, f_{11})")
pairwise.wilcox.test(BiObjBBOB1$HV, BiObjBBOB1$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB2$HV, BiObjBBOB2$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB11$HV, BiObjBBOB11$algorithm, p.adjust.method = "hommel")
# 
print("separable - moderate (f_3, f_4, f_{12}, f_{13})")
pairwise.wilcox.test(BiObjBBOB3$HV, BiObjBBOB3$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB4$HV, BiObjBBOB4$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB12$HV, BiObjBBOB12$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB13$HV, BiObjBBOB13$algorithm, p.adjust.method = "hommel")

print("separable - ill-conditioned (f_5, f_6, f_{14}, f_{15})")
pairwise.wilcox.test(BiObjBBOB5$HV, BiObjBBOB5$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB6$HV, BiObjBBOB6$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB14$HV, BiObjBBOB14$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB15$HV, BiObjBBOB15$algorithm, p.adjust.method = "hommel")

print("separable - multi-modal (f_7, f_8, f_{16}, f_{17})")
pairwise.wilcox.test(BiObjBBOB7$HV, BiObjBBOB7$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB8$HV, BiObjBBOB8$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB16$HV, BiObjBBOB16$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB17$HV, BiObjBBOB17$algorithm, p.adjust.method = "hommel")

print("separable - weakly-structured (f_9, f_{10}, f_{18}, f_{19})")
pairwise.wilcox.test(BiObjBBOB9$HV, BiObjBBOB9$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB10$HV, BiObjBBOB10$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB18$HV, BiObjBBOB18$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB19$HV, BiObjBBOB19$algorithm, p.adjust.method = "hommel")

print("moderate - moderate (f_{20}, f_{21}, f_{28})")
pairwise.wilcox.test(BiObjBBOB20$HV, BiObjBBOB20$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB21$HV, BiObjBBOB21$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB28$HV, BiObjBBOB28$algorithm, p.adjust.method = "hommel")

print("moderate - ill-conditioned (f_{22}, f_{23}, f_{29}, f_{30})")
pairwise.wilcox.test(BiObjBBOB22$HV, BiObjBBOB22$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB23$HV, BiObjBBOB23$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB29$HV, BiObjBBOB29$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB30$HV, BiObjBBOB30$algorithm, p.adjust.method = "hommel")

print("moderate - multi-modal (f_{24}, f_{25}, f_{31}, f_{32})")
pairwise.wilcox.test(BiObjBBOB24$HV, BiObjBBOB24$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB25$HV, BiObjBBOB25$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB31$HV, BiObjBBOB31$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB32$HV, BiObjBBOB32$algorithm, p.adjust.method = "hommel")

print("moderate - weakly-structured (f_{26}, f_{27}, f_{33}, f_{34})")
pairwise.wilcox.test(BiObjBBOB26$HV, BiObjBBOB26$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB27$HV, BiObjBBOB27$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB33$HV, BiObjBBOB33$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB34$HV, BiObjBBOB34$algorithm, p.adjust.method = "hommel")

print("ill-conditioned - ill-conditioned (f_{35}, f_{36}, f_{41})")
pairwise.wilcox.test(BiObjBBOB35$HV, BiObjBBOB35$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB36$HV, BiObjBBOB36$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB41$HV, BiObjBBOB41$algorithm, p.adjust.method = "hommel")

print("ill-conditioned - multi-modal (f_{37}, f_{38}, f_{42}, f_{43})")
pairwise.wilcox.test(BiObjBBOB37$HV, BiObjBBOB37$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB38$HV, BiObjBBOB38$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB42$HV, BiObjBBOB42$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB43$HV, BiObjBBOB43$algorithm, p.adjust.method = "hommel")

print("ill-conditioned - weakly-structured (f_{39}, f_{40}, f_{44}, f_{45})")
pairwise.wilcox.test(BiObjBBOB39$HV, BiObjBBOB39$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB40$HV, BiObjBBOB30$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB44$HV, BiObjBBOB44$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB45$HV, BiObjBBOB45$algorithm, p.adjust.method = "hommel")

print("multi-modal - multi-modal (f_{46}, f_{47}, f_{50})")
pairwise.wilcox.test(BiObjBBOB46$HV, BiObjBBOB46$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB47$HV, BiObjBBOB47$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB50$HV, BiObjBBOB50$algorithm, p.adjust.method = "hommel")

print("multi-modal - weakly structured (f_{48}, f_{49}, f_{51}, f_{52})")
pairwise.wilcox.test(BiObjBBOB48$HV, BiObjBBOB48$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB49$HV, BiObjBBOB49$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB51$HV, BiObjBBOB51$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB52$HV, BiObjBBOB52$algorithm, p.adjust.method = "hommel")

print("weakly structured - weakly structured (f_{53}, f_{54}, f_{55})")
pairwise.wilcox.test(BiObjBBOB53$HV, BiObjBBOB53$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB54$HV, BiObjBBOB54$algorithm, p.adjust.method = "hommel")
pairwise.wilcox.test(BiObjBBOB55$HV, BiObjBBOB55$algorithm, p.adjust.method = "hommel")

# 
aggregate(bbob.non$unlist.nondon., mean, by = list(bbob.non$algorithm1))
aggregate(bbob.non.sd$unlist.sd.nondon., mean, by = list(bbob.non.sd$algorithm1))
