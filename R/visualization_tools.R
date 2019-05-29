library(smoof)
library(MOEADr)
library(emoa)
library(stringr)
library(ecr)
library(mco)
library(feather)
library(pracma)
library(withr)
library(ggplot2)
library(plotly)


tool1 <- function(){
Resources <- Reduce("+", moead.rad$usage)
Subproblem <- 1:length(Resources)
b <- data.frame(Resources, Subproblem)
v <- ggplot(b, aes(Subproblem, Resources)) +geom_line()+ theme(axis.text = element_text(size =
                                                                                          14),
                                                               axis.title =
                                                                 element_text(size = 16, face = "bold")) + theme(plot.title = element_text(
                                                                   color = "blue",
                                                                   size = 24,
                                                                   face = "bold"
                                                                 ))+ geom_hline(yintercept = 200, colour = "red")+ ylim(0,400)
v +labs(title=paste0("MOEA/D-RAD - F44"))


Resources <- Reduce("+", moead.dra$usage)
Subproblem <- 1:length(Resources)
b <- data.frame(Resources, Subproblem)
v <- ggplot(b, aes(Subproblem, Resources)) +geom_line()+ theme(axis.text = element_text(size =
                                                                                          14),
                                                               axis.title =
                                                                 element_text(size = 16, face = "bold")) + theme(plot.title = element_text(
                                                                   color = "blue",
                                                                   size = 24,
                                                                   face = "bold"
                                                                 ))+ geom_hline(yintercept = 200, colour = "red")+ ylim(0,400)
v +labs(title=paste0("MOEA/D-DRA - F44"))
}

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

visuEvol <- function(moea, name, ref.front=NULL){
  curDir <- getwd()
  setwd("~/MOEADr/dataExp/")


pareto.front <- data.frame(moea$plot.paretofront)
resources <- data.frame(moea$plot.resources)
pareto.set <- data.frame(moea$plot.paretoset)
pareto.front$f1 <- as.numeric(pareto.front$f1)
pareto.front$f2 <- as.numeric(pareto.front$f2)
pareto.front$stage <- as.numeric(pareto.front$stage)
pareto.front$Nondominated <- as.numeric(pareto.front$non.dominated)
resources$Subproblem <- as.numeric(resources$Subproblem)
resources$Resources <- as.numeric(resources$Resources)

ncols <- ncol(pareto.front)-3
pareto.front[,1:ncols] <- scaling_Y(pareto.front[,1:ncols], pareto.front[,1:ncols])


# iteration.data <- cbind(pareto.set, pareto.front, resources)
iteration.data <- data.frame(cbind(pareto.front, resources, pareto.set))
iteration.data$stage <- unlist(iteration.data$stage)

# bad, do apply  on levels
# iteration.data$f1 <- (iteration.data$f1 - min(iteration.data$f1))/(max(iteration.data$f1) - min(iteration.data$f1))
# iteration.data$f2 <- (iteration.data$f2 - min(iteration.data$f2))/(max(iteration.data$f2) - min(iteration.data$f2))

df.pca <- data.frame()
iteration.data$stage <- unlist(iteration.data$stage)
# Yref <-
#   as.matrix(read.table(paste0("inst/extdata/pf_data/DTLZ4.2D.pf")))

hvs <- list(rep(0, max(iteration.data$stage)))
igds <- list(rep(0, max(iteration.data$stage)))
for (i in 1:max(iteration.data$stage)){
  Y <- rbind(iteration.data[iteration.data$stage==i,]$f1, iteration.data[iteration.data$stage==i,]$f2)
  hvs[[i]] <- emoa::dominated_hypervolume(points = Y, ref = rep(1.1,dim(moea$W)[2]))
}


for (i in 1:max(iteration.data$stage)){
  if (!is.null(ref.front)){
    Y <- rbind(iteration.data[iteration.data$stage==i,]$f1, iteration.data[iteration.data$stage==i,]$f2)
    igds[[i]] <- igd <- calcIGD(Y, Yref = ref.front)
  }else{
    igds[[i]] <- 0
  }
}


for (i in 1:max(iteration.data$stage)){
  temp <- iteration.data[iteration.data$stage==i,]
  temp <- temp[ ,(length(iteration.data)-length(pareto.set)+2):length(iteration.data)-1]
  paretoset <- matrix(unlist(temp[,-ncol(temp)]), ncol = ncol(temp)-1, nrow = nrow(temp))
  paretoset.pca <- prcomp(paretoset, center = TRUE, scale = TRUE)
  df.pca <- rbind(df.pca, data.frame(i, paretoset.pca$x[,1], paretoset.pca$x[,3]))

}

names(df.pca) <- c("stage", "Comp.1", "Comp.2")
df.pca$Nondominated <- as.numeric(iteration.data$non.dominated)
df.pca$Resources <- as.numeric(iteration.data$Resources)
df.pca$Subproblem <- as.numeric(iteration.data$Subproblem)

hvs <- data.frame(unlist(hvs), 1:max(iteration.data$stage))
names(hvs) <- c("hv", "stage") 
hvs2 <- hvs %>%
  accumulate_by(~stage)
hvs2$stage <- hvs2$frame


igds <- data.frame(unlist(igds), 1:max(iteration.data$stage))
names(igds) <- c("igd", "stage")
igds2 <- igds %>%
  accumulate_by(~stage)
igds2$stage <- igds2$frame

# resource/subproblem with size by by nondom
resource.problem <- iteration.data %>%
  plot_ly(
    x = ~ Subproblem,
    y = ~ Resources,
    frame = ~ stage,
    type = 'scatter',
    mode = 'markers',
    showlegend = F,
    colors = c('black', 'blue'),
    color = ~ Nondominated,
    alpha = 0.5,
    # marker = list(size = ~ (Nondominated+1)*5, opacity = 0.5),
    stroke = I("black"),
    strokes = ~ Nondominated,
    text = ~ paste("Nondominated: ", Nondominated, '<br>Subproblem:', Subproblem), 
    hoverinfo = 'text'
  ) %>%
  layout(title = "Line 1: (1)-RA (2)-PF (size: resource) (3)-PF (size by nondominated) <br> Line 2; (1)-HV (2)-IGD (3)-PS-PCA"
    ) 

# pareto front with size by nondominated
pf.nondominated <- iteration.data %>%
  plot_ly(
    x = ~ f1,
    y = ~ f2,
    frame = ~ stage,
    type = 'scatter',
    mode = 'markers',
    showlegend = F,
    colors = c('black', 'blue'),
    color = ~ Nondominated,
    stroke = I("black"),
    strokes = ~ Nondominated,
    marker = list(size = ~ (Nondominated+1)*5, opacity = 0.5),
    text = ~ paste("Nondominated: ", Nondominated, '<br>Subproblem:', Subproblem), 
    hoverinfo = 'text'
  ) 

# pareto front with size by resource
pf.resource <- iteration.data %>%
  plot_ly(
    x = ~ f1,
    y = ~ f2,
    frame = ~ stage,
    type = 'scatter',
    mode = 'markers',
    showlegend = F,
    colors = c('black', 'blue'),
    color = ~ Nondominated,
    stroke = I("black"),
    strokes = ~ Nondominated,
    marker = list(size = ~ log(Resources+1)*5, opacity = 0.5),
    text = ~ paste("Nondominated: ", Nondominated, '<br>Subproblem:', Subproblem), 
    hoverinfo = 'text'
  ) 

# pareto set
ps.pca <- df.pca %>%
  plot_ly(
    x = ~ Comp.1,
    y = ~ Comp.2,
    frame = ~ stage,
    type = 'scatter',
    mode = 'markers',
    showlegend = F,
    colors = c('black', 'blue'),
    color = ~ Nondominated,
    alpha=0.5,
    marker = list(size = ~ log(Resources+1)*5, opacity = 0.5),
    text = ~ paste("Nondominated: ", Nondominated, '<br>Subproblem:', Subproblem),
    hoverinfo = 'text'
  )



hv.plot <- hvs2 %>%
  plot_ly(
    y = ~ hv,
    frame = ~ stage,
    type = 'scatter',
    mode = 'lines',
    showlegend = F,
    colors = c('black', 'blue'),
    fill = 'tozeroy', 
    fillcolor='rgba(58, 83, 155, 0.5)',
    line = list(color = 'rgba(58, 83, 155, 1)')
    ) %>%
  layout(
    xaxis = list(
      # title = "Day",
      range = c(0,max(hvs2$stage)),
      zeroline = F,
      showgrid = F
    ),yaxis = list(
      # title = "Day",
      range = c(0,max(hvs2$hv)*1.2),
      zeroline = F,
      showgrid = F
    )
    
  ) %>%
  animation_opts(
    frame = 100,
    transition = 0,
    redraw = FALSE
  ) 


igd.plot <- igds2 %>%
  plot_ly(
    y = ~ igd,
    frame = ~ stage,
    type = 'scatter',
    mode = 'lines',
    showlegend = F,
    colors = c('black', 'blue'),
    fill = 'tozeroy',
    fillcolor='rgba(58, 83, 155, 0.5)',
    line = list(color = 'rgba(58, 83, 155, 1)')
    ) %>%
  layout(
    xaxis = list(
      title = "Day",
      range = c(0,max(hvs2$stage)),
      zeroline = F,
      showgrid = F
    )
  ) %>%
  animation_opts(
    frame = 100,
    transition = 0,
    redraw = FALSE
  )

p <- subplot(resource.problem, pf.resource, pf.nondominated, hv.plot, igd.plot, ps.pca, nrows = 2)%>%
  animation_slider(
    currentvalue = list(
      prefix = "Iteration "
    )
  )
htmlwidgets::saveWidget(p, name)
setwd(curDir)
}


savePlotData <- function (moea, name, j){
  curDir <- getwd()
  setwd("~/MOEADr/R/")
  write_feather(data.frame(moea$X), paste0('../dataExp/', name, j,'_X'))
  write_feather(data.frame(moea$Y), paste0('../dataExp/', name, j,'_Y'))
  
  temp <- moea$plot.paretofront[-1,]
  temp <- apply(temp, 2, unlist)
  temp<-as.data.frame(temp)
  write_feather(temp, paste0('../dataExp', name, j,'_plot.paretofront'))
  
  temp <- as.data.frame(moea$plot.resources[-1,])
  temp <- apply(temp, 2, unlist)
  temp<-as.data.frame(temp)
  write_feather(temp, paste0('../dataExp/', name, j,'_plot.resources'))
  
  temp <- as.data.frame(moea$plot.paretoset[-1,])
  temp <- apply(temp, 2, unlist)
  temp<-as.data.frame(temp)
  write_feather(temp, paste0('../dataExp/', name, j,'_plot.paretoset'))
  
  temp <- as.data.frame(moea$W)
  temp <- apply(temp, 2, unlist)
  temp<-as.data.frame(temp)
  write_feather(temp, paste0('../dataExp/', name, j,'_W'))
  
  temp <- as.data.frame(moea$nfe)
  temp <- apply(temp, 2, unlist)
  temp<-as.data.frame(temp)
  write_feather(temp, paste0('../dataExp/', name, j,'_nfe'))
  
  temp <- as.data.frame(moea$n.iter)
  temp <- apply(temp, 2, unlist)
  temp<-as.data.frame(temp)
  write_feather(temp, paste0('../dataExp/', name, j,'_iter'))
  
  temp <- as.data.frame(moea$time)
  temp <- apply(temp, 2, unlist)
  temp<-as.data.frame(temp)
  write_feather(temp, paste0('../dataExp/', name, j,'_time'))
  setwd(curDir)
}

loadPlotData <- function (name, j){
  curDir <- getwd()
  setwd("~/MOEADr/R/")
  X <- read_feather(paste0('../dataExp/', name, j,'_X'))
  Y <- read_feather(paste0('../dataExp/', name, j,'_Y'))
  plot.paretofront <- read_feather(paste0('../dataExp', name, j,'_plot.paretofront'))
  plot.resources <- read_feather(paste0('../dataExp/', name, j,'_plot.resources'))
  plot.paretoset <- read_feather(paste0('../dataExp/', name, j,'_plot.paretoset'))
  iter <- read_feather(paste0('../dataExp/', name, j,'_iter'))
  time <- read_feather(paste0('../dataExp/', name, j,'_time'))
  W <- read_feather(paste0('../dataExp/', name, j,'_W'))
  nfe <- read_feather(paste0('../dataExp/', name, j,'_nfe'))
  
  out <- list(
    X           = X,
    Y           = Y,
    W           = W,
    nfe         = nfe,
    n.iter      = iter,
    time        = time,
    plot.paretofront = plot.paretofront,
    plot.paretoset = plot.paretoset,
    plot.resources = plot.resources,
    moead.norm = FALSE
  )
  return(out)
  setwd(curDir)
}


