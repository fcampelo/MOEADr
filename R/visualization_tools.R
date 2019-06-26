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


tool1 <- function() {
  Resources <- Reduce("+", moead.norm$usage)
  Subproblem <- 1:length(Resources)
  b <- data.frame(Resources, Subproblem)
  v <-
    ggplot(b, aes(Subproblem, Resources)) + geom_line(size = 2) + theme(axis.text = element_text(size =
                                                                                           14*3),
                                                                axis.title =
                                                                  element_text(size = 16*3, face = "bold")) + theme(plot.title = element_text(
                                                                    color = "blue",
                                                                    size = 24*3,
                                                                    face = "bold"
                                                                  )) + geom_hline(yintercept = 200, colour = "red") + ylim(0, 400)
  v + labs(title = paste0("MOEA/D 2-Norm - UF9"))
  
  
  Resources <- Reduce("+", moead.gra$usage)
  Subproblem <- 1:length(Resources)
  b <- data.frame(Resources, Subproblem)
  v <-
    ggplot(b, aes(Subproblem, Resources)) + geom_line(size = 2) + theme(axis.text = element_text(size =
                                                                                           14*3),
                                                                axis.title =
                                                                  element_text(size = 16*3, face = "bold")) + theme(plot.title = element_text(
                                                                    color = "blue",
                                                                    size = 24*3,
                                                                    face = "bold"
                                                                  )) + geom_hline(yintercept = 200, colour = "red") + ylim(0, 400)
  v + labs(title = paste0("MOEA/D R.I. - UF9"))
}

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)],], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

saveWidgetFix <- function (widget,file,...) {
  ## A wrapper to saveWidget which compensates for arguable BUG in
  ## saveWidget which requires `file` to be in current working
  ## directory.
  wd<-getwd()
  on.exit(setwd(wd))
  outDir<-dirname(file)
  file<-basename(file)
  setwd(outDir);
  htmlwidgets::saveWidget(widget,file=file,...)
}

visuEvol <- function(moea,
                     name,
                     fun,
                     ref.front = NULL,
                     test = TRUE) {

  
  pareto.front <- data.frame(moea$plot.paretofront)
  resources <- data.frame(moea$plot.resources)
  # pareto.set <- data.frame(moea$plot.paretoset)
  pareto.front$f1 <- as.numeric(pareto.front$f1)
  pareto.front$f2 <- as.numeric(pareto.front$f2)
  
  pareto.front$stage <- as.numeric(pareto.front$stage)
  pareto.front$Nondominated <- as.numeric(pareto.front$non.dominated)
  resources$Subproblem <- as.numeric(resources$Subproblem)
  resources$Resources <- as.numeric(resources$Resources)
  
  ncols <- ncol(pareto.front) - 3
  
  
  # iteration.data <-
  #   data.frame(cbind(pareto.front, resources, pareto.set))
  iteration.data <-
    data.frame(cbind(pareto.front, resources))
  iteration.data$stage <- unlist(iteration.data$stage)
  if(strsplit(fun, "[0-9]")[[1]] == "DTLZ"){
    Yref <-
      as.matrix(read.table(paste0(
        "~/MOEADr/inst/extdata/pf_data/", fun, ".2D.pf"
      )))
  }
  if(strsplit(fun, "[0-9]")[[1]] == "UF"){
    Yref <-
      as.matrix(read.table(paste0(
        "~/MOEADr/inst/extdata/pf_data/", fun, ".pf"
      )))
  }
  
  temp <- data.frame(Yref)
  
  g<-ggplot(temp, aes(x=V1, y=V2))+geom_line()
  
  hvs <- list(rep(0, max(iteration.data$stage)))
  igds <- list(rep(0, max(iteration.data$stage)))
  
  for (i in 1:max(iteration.data$stage)) {
    if (!is.null(ref.front)) {
      Y <-
        rbind(iteration.data[iteration.data$stage == i, ]$f1, iteration.data[iteration.data$stage ==
                                                                               i, ]$f2)
      igds[[i]] <- igd <- calcIGD(Y, Yref = Yref)
    } else{
      igds[[i]] <- -1
    }
  }
  
  iteration.data[, 1:2] <-
    scaling_Y(iteration.data[, 1:2], ref1)
  
  for (i in 1:max(iteration.data$stage)) {
    Y <-
      rbind(iteration.data[iteration.data$stage == i, ]$f1, iteration.data[iteration.data$stage ==
                                                                             i, ]$f2)
    hvs[[i]] <-
      emoa::dominated_hypervolume(points = Y, ref = rep(1, dim(moea$W)[2]))
  }
  
  
  hvs <- data.frame(unlist(hvs), 1:max(iteration.data$stage))
  names(hvs) <- c("hv", "stage")
  hvs2 <- hvs %>%
    accumulate_by( ~ stage)
  hvs2$stage <- hvs2$frame
  
  
  igds <- data.frame(unlist(igds), 1:max(iteration.data$stage))
  names(igds) <- c("igd", "stage")
  igds2 <- igds %>%
    accumulate_by( ~ stage)
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
      colors = (palette = "Spectral"),
      color = ~ Subproblem,
      #alpha = 0.5,
      width = 900, height = 900,
      marker = list(size = ~ (Nondominated + 1) * 5, opacity = 0.5),
      stroke = I("black"),
      strokes = ~ Nondominated,
      text = ~ paste(
        "Nondominated: ",
        Nondominated,
        '<br>Subproblem:',
        Subproblem
      ),
      hoverinfo = 'text'
    ) %>%
    layout(yaxis = list(
      zeroline = F,
      range = c(0, max(iteration.data$Resources)*1.2),
      showgrid = F)
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
      width = 900, height = 900,
      colors = (palette = "OrRd"),
      color = ~ as.factor(Nondominated),
      stroke = I("black"),
      strokes = ~ Nondominated,
      alpha = 0.5,
      text = ~ paste(
        "Nondominated: ",
        Nondominated,
        '<br>Subproblem:',
        Subproblem
      ),
      hoverinfo = 'text'
    ) %>%
    layout(yaxis = list(
      range = c(-0.1, 1.2),
      zeroline = F,
      showgrid = F),
      xaxis = list(
        range = c(-0.1, 1.2)
      )
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
      width = 900, height = 900,
      colors = (palette = "Spectral"),
      color = ~ Subproblem,
      stroke = I("black"),
      strokes = ~ Nondominated,
      alpha = 0.5,
      text = ~ paste(
        "Nondominated: ",
        Nondominated,
        '<br>Subproblem:',
        Subproblem
      ),
      hoverinfo = 'text'
    )%>%
    layout(yaxis = list(
      range = c(-0.1, 1.2),
      zeroline = F,
      showgrid = F),
      xaxis = list(
        range = c(-0.1, 1.2)
      )
    )
  
  
  hv.plot <- hvs2 %>%
    plot_ly(
      y = ~ hv,
      frame = ~ stage,
      type = 'scatter',
      mode = 'lines',
      showlegend = F,
      width = 900, height = 900,
      colors = c('black', 'blue'),
      fill = 'tozeroy',
      fillcolor = 'rgba(58, 83, 155, 0.5)',
      line = list(color = 'rgba(58, 83, 155, 1)')
    ) %>%
    layout(
      xaxis = list(
        range = c(0, max(hvs2$stage)),
        zeroline = F,
        showgrid = F
      ),
      yaxis = list(
        range = c(0, max(hvs2$hv) * 1.2),
        zeroline = F,
        showgrid = F
      )
      
    ) %>%
    animation_opts(frame = 100,
                   transition = 0,
                   redraw = FALSE)
  
  
  igd.plot <- igds2 %>%
    plot_ly(
      y = ~ igd,
      frame = ~ stage,
      type = 'scatter',
      mode = 'lines',
      showlegend = F,
      width = 900, height = 900,
      colors = c('black', 'blue'),
      fill = 'tozeroy',
      fillcolor = 'rgba(58, 83, 155, 0.5)',
      line = list(color = 'rgba(58, 83, 155, 1)')
    ) %>%
    layout(xaxis = list(
      range = c(0, max(igds2$stage)),
      zeroline = F,
      showgrid = F
    )) %>%
    animation_opts(frame = 100,
                   transition = 0,
                   redraw = FALSE)
  
  p <-
    subplot(resource.problem,
            pf.resource,
            pf.nondominated,
            hv.plot,
            igd.plot,
            # g,
            nrows = 2) %>%
    animation_slider(currentvalue = list(prefix = "Iteration ", font = list(color="red")), redraw = FALSE, frame = 100) %>% layout(title = paste(fun, name))
  return(p)
}


savePlotData <- function (moea, name, j) {
  if (is.null(moea$Archive)) {
    write_feather(data.frame(moea$X), paste0('../dataExp/', name, j, '_X'))
    write_feather(data.frame(moea$Y), paste0('../dataExp/', name, j, '_Y'))
  }
  else{
    write_feather(data.frame(moea$Archive$X),
                  paste0('../dataExp/', name, j, '_X'))
    write_feather(data.frame(moea$Archive$Y),
                  paste0('../dataExp/', name, j, '_Y'))
  }
  
  
  temp <- moea$plot.paretofront[-1, ]
  temp <- apply(temp, 2, unlist)
  temp <- as.data.frame(temp)
  write_feather(temp, paste0('../dataExp/', name, j, '_plot.paretofront'))
  
  temp <- as.data.frame(moea$plot.resources[-1, ])
  temp <- apply(temp, 2, unlist)
  temp <- as.data.frame(temp)
  write_feather(temp, paste0('../dataExp/', name, j, '_plot.resources'))
  
  # temp <- as.data.frame(moea$plot.paretoset[-1, ])
  # temp <- apply(temp, 2, unlist)
  # temp <- as.data.frame(temp)
  # write_feather(temp, paste0('../dataExp/', name, j, '_plot.paretoset'))
  
  temp <- as.data.frame(moea$W)
  temp <- apply(temp, 2, unlist)
  temp <- as.data.frame(temp)
  write_feather(temp, paste0('~/MOEADr/dataExp/', name, j, '_W'))
  
  temp <- as.data.frame(moea$nfe)
  temp <- apply(temp, 2, unlist)
  temp <- as.data.frame(temp)
  write_feather(temp, paste0('~/MOEADr/dataExp/', name, j, '_nfe'))
  
  temp <- as.data.frame(moea$n.iter)
  temp <- apply(temp, 2, unlist)
  temp <- as.data.frame(temp)
  write_feather(temp, paste0('~/MOEADr/dataExp/', name, j, '_iter'))
  
  temp <- as.data.frame(moea$time)
  temp <- apply(temp, 2, unlist)
  temp <- as.data.frame(temp)
  write_feather(temp, paste0('~/MOEADr/dataExp/', name, j, '_time'))
  
}

loadPlotData <- function (name, j) {
  
  X <- read_feather(paste0('~/MOEADr/dataExp/', name, j, '_X'))
  Y <- read_feather(paste0('~/MOEADr/dataExp/', name, j, '_Y'))
  plot.paretofront <-
    read_feather(paste0('~/MOEADr/dataExp/', name, j, '_plot.paretofront'))
  plot.resources <-
    read_feather(paste0('~/MOEADr/dataExp/', name, j, '_plot.resources'))
  # plot.paretoset <-
  #   read_feather(paste0('../dataExp/', name, j, '_plot.paretoset'))
  iter <- read_feather(paste0('~/MOEADr/dataExp/', name, j, '_iter'))
  time <- read_feather(paste0('~/MOEADr/dataExp/', name, j, '_time'))
  W <- read_feather(paste0('~/MOEADr/dataExp/', name, j, '_W'))
  nfe <- read_feather(paste0('~/MOEADr/dataExp/', name, j, '_nfe'))
  
  out <- list(
    X           = X,
    Y           = Y,
    W           = W,
    nfe         = nfe,
    n.iter      = iter,
    time        = time,
    #Vmatrix = Vmatrix,
    plot.paretofront = plot.paretofront,
    # plot.paretoset = plot.paretoset,
    plot.resources = plot.resources,
    moead.norm = FALSE
  )
  return(out)

}
