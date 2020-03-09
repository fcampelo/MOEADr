library("dplyr")
library(emoa)
library(ecr)
library(eaf)
library(feather)
library(ggplot2)
require(gridExtra)
#library(gganimate)
#library(hrbrthemes)
#library(ks)
#require(kohonen)
library(MOEADr)
library(mco)
library(MASS)
#library(pracma)
library(plyr) # used
library(plotly)
require(RColorBrewer)
library(stringr)
library(smoof)
# library(tidyverse) # used
#library(viridis)
library(withr)


tool1 <- function() {
  Resources <- Reduce("+", moead.norm$usage)
  Subproblem <- 1:length(Resources)
  b <- data.frame(Resources, Subproblem)
  v <-
    ggplot(b, aes(Subproblem, Resources)) + geom_line(size = 2) + theme(axis.text = element_text(size =
                                                                                                   14 *
                                                                                                   3),
                                                                        axis.title =
                                                                          element_text(size = 16 *
                                                                                         3, face = "bold")) + theme(plot.title = element_text(
                                                                                           color = "blue",
                                                                                           size = 24 *
                                                                                             3,
                                                                                           face = "bold"
                                                                                         )) + geom_hline(yintercept = 200, colour = "red") + ylim(0, 400)
  v + labs(title = paste0("MOEA/D 2-Norm - UF9"))
  
  
  Resources <- Reduce("+", moead.gra$usage)
  Subproblem <- 1:length(Resources)
  b <- data.frame(Resources, Subproblem)
  v <-
    ggplot(b, aes(Subproblem, Resources)) + geom_line(size = 2) + theme(axis.text = element_text(size =
                                                                                                   14 *
                                                                                                   3),
                                                                        axis.title =
                                                                          element_text(size = 16 *
                                                                                         3, face = "bold")) + theme(plot.title = element_text(
                                                                                           color = "blue",
                                                                                           size = 24 *
                                                                                             3,
                                                                                           face = "bold"
                                                                                         )) + geom_hline(yintercept = 200, colour = "red") + ylim(0, 400)
  v + labs(title = paste0("MOEA/D R.I. - UF9"))
}

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

saveWidgetFix <- function (widget, file, ...) {
  ## A wrapper to saveWidget which compensates for arguable BUG in
  ## saveWidget which requires `file` to be in current working
  ## directory.
  wd <- getwd()
  on.exit(setwd(wd))
  outDir <- dirname(file)
  file <- basename(file)
  setwd(outDir)
  
  htmlwidgets::saveWidget(widget, file = file, ...)
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
  pareto.front$Nondominated <-
    as.numeric(pareto.front$non.dominated)
  resources$Subproblem <- as.numeric(resources$Subproblem)
  resources$Resources <- as.numeric(resources$Resources)
  
  ncols <- ncol(pareto.front) - 3
  
  
  # iteration.data <-
  #   data.frame(cbind(pareto.front, resources, pareto.set))
  iteration.data <-
    data.frame(cbind(pareto.front, resources))
  iteration.data$stage <- unlist(iteration.data$stage)
  if (strsplit(fun, "[0-9]")[[1]] == "DTLZ") {
    Yref <-
      as.matrix(read.table(paste0(
        "~/inst/extdata/pf_data/", fun, ".2D.pf"
      )))
  }
  if (strsplit(fun, "[0-9]")[[1]] == "UF") {
    Yref <-
      as.matrix(read.table(paste0(
        "~/inst/extdata/pf_data/", fun, ".pf"
      )))
  }
  
  # temp <- data.frame(Yref)
  
  # g<-ggplot(temp, aes(x=V1, y=V2))+geom_line()
  
  hvs <- list(rep(0, max(iteration.data$stage)))
  igds <- list(rep(0, max(iteration.data$stage)))
  
  for (i in 1:max(iteration.data$stage)) {
    if (!is.null(ref.front)) {
      Y <-
        cbind(iteration.data[iteration.data$stage == i,]$f1, iteration.data[iteration.data$stage ==
                                                                              i,]$f2)
      igds[[i]] <- calcIGD(Y, Yref = Yref)
    } else{
      igds[[i]] <- -1
    }
  }
  
  # iteration.data[, 1:2] <-
  #   scaling_Y(iteration.data[, 1:2], ref1)
  
  for (i in 1:max(iteration.data$stage)) {
    Y <-
      rbind(iteration.data[iteration.data$stage == i,]$f1, iteration.data[iteration.data$stage ==
                                                                            i,]$f2)
    hvs[[i]] <-
      emoa::dominated_hypervolume(points = Y, ref = c(max(ref.front[, 1]), max(ref.front[, 2])))
    # emoa::dominated_hypervolume(points = Y, ref = rep(1, dim(moea$W)[2]))
  }
  
  hvs <- data.frame(unlist(hvs), 1:max(iteration.data$stage))
  names(hvs) <- c("hv", "stage")
  hvs2 <- hvs %>%
    accumulate_by(~ stage)
  hvs2$stage <- hvs2$frame
  
  
  igds <- data.frame(unlist(igds), 1:max(iteration.data$stage))
  names(igds) <- c("igd", "stage")
  igds2 <- igds %>%
    accumulate_by(~ stage)
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
      width = 900,
      height = 900,
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
      range = c(0, max(iteration.data$Resources) * 1.2),
      showgrid = F
    ))
  
  # pareto front with size by nondominated
  pf.nondominated <- iteration.data %>%
    plot_ly(
      x = ~ f1,
      y = ~ f2,
      frame = ~ stage,
      type = 'scatter',
      mode = 'markers',
      showlegend = F,
      width = 900,
      height = 900,
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
      showgrid = F
    ),
    xaxis = list(range = c(-0.1, 1.2)))
  
  # pareto front with size by resource
  pf.resource <- iteration.data %>%
    plot_ly(
      x = ~ f1,
      y = ~ f2,
      frame = ~ stage,
      type = 'scatter',
      mode = 'markers',
      showlegend = F,
      width = 900,
      height = 900,
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
    ) %>%
    layout(yaxis = list(
      range = c(-0.1, 1.2),
      zeroline = F,
      showgrid = F
    ),
    xaxis = list(range = c(-0.1, 1.2)))
  
  
  hv.plot <- hvs2 %>%
    plot_ly(
      y = ~ hv,
      frame = ~ stage,
      type = 'scatter',
      mode = 'lines',
      showlegend = F,
      width = 900,
      height = 900,
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
      width = 900,
      height = 900,
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
    animation_slider(
      currentvalue = list(prefix = "Iteration ", font = list(color = "red")),
      redraw = FALSE,
      frame = 100
    ) %>% layout(title = paste(fun, name), plot_bgcolor = "lightgray")
  return(p)
}


savePlotData <- function (moea, name, j,wd = '~/dataExp/') {
  if (is.null(moea$Archive)) {
    write_feather(data.frame(moea$X), paste0(wd, name, j, '_X'))
    write_feather(data.frame(moea$Y), paste0(wd, name, j, '_Y'))
  }
  else{
    write_feather(data.frame(moea$Archive$X),
                  paste0('~/dataExp/', name, j, '_X'))
    write_feather(data.frame(moea$Archive$Y),
                  paste0('~/dataExp/', name, j, '_Y'))
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
  
  # temp <- as.data.frame(moea$plot.paretoset[-1, ])
  # temp <- apply(temp, 2, unlist)
  # temp <- as.data.frame(temp)
  # write_feather(temp, paste0('~/dataExp/', name, j, '_plot.paretoset'))
  
  temp <- as.data.frame(moea$W)
  temp <- apply(temp, 2, unlist)
  temp <- as.data.frame(temp)
  write_feather(temp, paste0(wd, name, j, '_W'))
  
  temp <- as.data.frame(moea$usage)
  temp <- apply(temp, 2, unlist)
  temp <- as.data.frame(temp)
  write_feather(temp, paste0(wd, name, j, '_usage'))
  
  temp <- as.data.frame(moea$nfe)
  temp <- apply(temp, 2, unlist)
  temp <- as.data.frame(temp)
  write_feather(temp, paste0(wd, name, j, '_nfe'))
  
  temp <- as.data.frame(moea$n.iter)
  temp <- apply(temp, 2, unlist)
  temp <- as.data.frame(temp)
  write_feather(temp, paste0(wd, name, j, '_iter'))
  
  temp <- as.data.frame(moea$time)
  temp <- apply(temp, 2, unlist)
  temp <- as.data.frame(temp)
  write_feather(temp, paste0(wd, name, j, '_time'))
  
}

loadPlotData <- function (name, j,wd='~/dataExp/') {
  X <- read_feather(paste0(wd, name, j, '_X'))
  Y <- read_feather(paste0(wd, name, j, '_Y'))
  # plot.paretofront <-
  #   read_feather(paste0(wd, name, j, '_plot.paretofront'))
  # plot.resources <-
  #   read_feather(paste0(wd, name, j, '_plot.resources'))
  iter <- read_feather(paste0(wd, name, j, '_iter'))
  usage <- read_feather(paste0(wd, name, j, '_usage'))
  time <- read_feather(paste0(wd, name, j, '_time'))
  W <- read_feather(paste0(wd, name, j, '_W'))
  nfe <- read_feather(paste0(wd, name, j, '_nfe'))
  
  out <- list(
    X           = X,
    Y           = Y,
    W           = W,
    nfe         = nfe,
    n.iter      = iter,
    time        = time,
    usage        = usage,
    #Vmatrix = Vmatrix,
    # plot.paretofront = plot.paretofront,
    
    # plot.resources = plot.resources,
    moead.norm = FALSE
  )
  return(out)
  
}

plot_density_estimation <-
  function(data1, Yref, n, name1, fun, wd = "~/Desktop/") {
    out <- savefile_create_fixs(name1)
    prefix1 <- out$prefix1
    posfix1 <- out$posfix1
    
    H <- Hscv(x = data1[, 1:n])
    fhat.data1 <- kde(x = data1[, 1:n], H = H)
    
    if (n == 3) {
      plot1 <-
        plot(
          fhat.data1,
          display = "filled.contour2",
          ylim = c(0, 1),
          xlim = c(0, 1),
          main = paste0(name1, " - Objective Space")
        )
      
      rgl_init()
      plot1 <-
        plot(
          fhat.data1,
          display = "filled.contour2",
          ylim = c(0, 1),
          xlim = c(0, 1),
          main = paste0(name1, " - Objective Space")
        )
      rgl.snapshot(paste0(
        wd,
        fun,
        "kernel_estimation_15_20_",
        prefix1,
        posfix1,
        ".png"
      ))
      
      rgl_init(theta = 40)
      plot1 <-
        plot(
          fhat.data1,
          display = "filled.contour2",
          ylim = c(0, 1),
          xlim = c(0, 1),
          main = paste0(name1, " - Objective Space")
        )
      rgl.snapshot(paste0(
        wd,
        fun,
        "kernel_estimation_40_20_",
        prefix1,
        posfix1,
        ".png"
      ))
      
      rgl_init(theta = 80, phi = 80)
      plot1 <-
        plot(
          fhat.data1,
          display = "filled.contour2",
          ylim = c(0, 1),
          xlim = c(0, 1),
          main = paste0(name1, " - Objective Space")
        )
      
      rgl.snapshot(paste0(
        wd,
        fun,
        "kernel_estimation_80_80_",
        prefix1,
        posfix1,
        ".png"
      ))
      rgl_init()
      plot2 <-
        plot(
          fhat.data2,
          display = "filled.contour2",
          ylim = c(0, 1),
          xlim = c(0, 1),
          main = paste0(name2, " - Objective Space"),
          size = 10
        )
      
    }
    if (n == 2) {
      png(
        filename = paste0(wd, fun, "kernel_estimation", prefix1, posfix1, ".png"),
        width = 960
      )
      
      plot1 <-
        plot(
          fhat.data1,
          display = "filled.contour2",
          ylim = c(0, 1),
          xlim = c(0, 1),
          main = paste0(name1, " - Objective Space")
        )
      points(Yref)
      
      dev.off()
    }
  }

plot_pca <-
  function(data1, name1, fun, wd = "~/Desktop/") {
    out <- savefile_create_fixs(name1, name2)
    prefix1 <- out$prefix1
    posfix1 <- out$posfix1
    
    set.seed(63 / 56)
    X.pca <- prcomp(data1[, 1:n], center = TRUE, scale. = TRUE)
    scores = as.data.frame(X.pca$x)
    plot1 <- ggplot(data = scores, aes(x = PC1, y = PC2))
    plot1 <-
      plot1 + geom_point() + xlim(-5, 5) + ylim(-5, 5) + labs(title = paste0(name1, "- Search Space"))
    
    # X2.pca <- prcomp(data2[, 1:n], center = TRUE, scale. = TRUE)
    # scores2 = as.data.frame(X2.pca$x)
    # plot2 <- ggplot(data = scores2, aes(x = PC1, y = PC2))
    # plot2 <-
    #   plot2 + geom_point() + xlim(-5, 5) + ylim(-5, 5) + labs(title = paste0(name2, "- Search Space"))
    
    png(
      filename = paste0(wd, fun, "pca", prefix1, posfix1, ".png"),
      width = 960
    )
    # png(filename = "~/Desktop/pca.png", width = 960)
    grid.arrange(plot1, ncol = 1)
    dev.off()
  }

plot_som <-
  function(data1, data2, name1, name2, fun, wd = "~/Desktop/") {
    out <- savefile_create_fixs(name1, name2)
    prefix1 <- out$prefix1
    posfix1 <- out$posfix1
    prefix2 <- out$prefix2
    posfix2 <- out$posfix2
    
    data_train_matrix <- scale(as.matrix(data1))
    som_grid <-
      somgrid(
        xdim = 20,
        ydim = 20,
        topo = "hexagonal",
        toroidal = TRUE,
        neighbourhood.fct = "gaussian"
      )
    set.seed(63 / 56)
    som_model.X <- som(data_train_matrix,
                       grid = som_grid, rlen = 500)
    
    data_train_matrix2 <- scale(as.matrix(data2))
    
    som_grid <-
      somgrid(
        xdim = 20,
        ydim = 20,
        topo = "hexagonal",
        toroidal = TRUE,
        neighbourhood.fct = "gaussian"
      )
    set.seed(63 / 56)
    som_model.X2 <- supersom(data_train_matrix2,
                             grid = som_grid, rlen = 500)
    png(
      filename = paste0(
        wd,
        fun,
        "som_model_mapping_distneighbours",
        prefix1,
        posfix1,
        "_",
        prefix2,
        posfix2,
        ".png"
      ),
      width = 960,
      height = 960
    )
    # png(filename = "~/Desktop/som_model_mapping_distneighbours.png",
    #     width = 960,
    #     height = 960)
    par(mfrow = c(1, 2))
    plot(
      som_model.X,
      type = "dist.neighbours",
      main = paste0(name1, " - Search Space"),
      palette.name = terrain.colors
    )
    plot(
      som_model.X2,
      type = "dist.neighbours",
      main = paste0(name2, " - Search Space"),
      palette.name = terrain.colors
    )
    #
    #
    # # dev.off()
    #
    # # reverse color ramp
    colors <- function(n, alpha = 1) {
      rev(heat.colors(n, alpha))
    }
    #
    # plot(
    #   som_model.X,
    #   type = "counts",
    #   palette.name = colors,
    #   heatkey = TRUE
    # )
    # plot(
    #   som_model.X2,
    #   type = "counts",
    #   palette.name = colors,
    #   heatkey = TRUE
    # )
    #
    # # png(filename="~/Desktop/mapping.png", width = 960)
    # # par(mfrow=c(1,2))
    # plot(som_model.X,
    #      type = "mapping",
    #      pchs = 20,
    #      main = "Fixed 5% - Search Space")
    # plot(som_model.X2,
    #      type = "mapping",
    #      pchs = 20,
    #      main = "Fixed 10% - Search Space")
    plot(
      som_model.X,
      type = "property",
      property = data_train_matrix[, 1],
      palette.name = colors
    )
    plot(
      som_model.X,
      type = "property",
      property = data_train_matrix[, 2],
      palette.name = colors
    )
    plot(
      som_model.X2,
      type = "property",
      property = data_train_matrix2[, 1],
      palette.name = colors
    )
    plot(
      som_model.X2,
      type = "property",
      property = data_train_matrix2[, 2],
      palette.name = colors
    )
    dev.off()
  }


plot_parcoord <-
  function(data1, Yref, n,  name1, fun, wd = "~/Desktop/") {
    my_max <- apply(Yref, 2, max)
    my_min <- apply(Yref, 2, min)
    legendtitle <-
      list(
        yref = 'paper',
        xref = "paper",
        y = 1.05,
        x = 1.1,
        text = "Cylinders",
        showarrow = F
      )
    p1 <- data1 %>%
      plot_ly(type = 'parcoords',
              dimensions = list(
                list(
                  range = c(0, 1),
                  constraintrange = c(my_min[1], my_max[1]),
                  label = 'F1',
                  values = ~ data1$f1
                ),
                list(
                  range = c(0, 1),
                  constraintrange = c(my_min[2], my_max[2]),
                  label = 'F2',
                  values = ~ data1$f2
                )
              )) %>%
      layout(title = name1, annotations = legendtitle)
    # p1
    cur_wd <- getwd()
    setwd(wd)
    orca(p1, paste0(fun, "parcoord", name1, "_", ".png"))
    setwd(cur_wd)
  }

plot_eaf_eafdiff <-
  function(data1, data2, n, name1, name2, fun, wd = "~/Desktop/") {
    out <- savefile_create_fixs(name1)
    prefix1 <- out$prefix1
    posfix1 <- out$posfix1
    out <- savefile_create_fixs(name2)
    prefix2 <- out$prefix1
    posfix2 <- out$posfix1
    
    if (n == 3) {
      Y.pca.1 <-
        prcomp(data1[, 1:n], center = TRUE, scale. = TRUE)
      Y.pca.2 <-
        prcomp(data2[, 1:n], center = TRUE, scale. = TRUE)
      Fixed_0.05_Y <- as.data.frame(Y.pca.1$x[, 1:2])
      Fixed_0.10_Y <- as.data.frame(Y.pca.2$x[, 1:2])
      Fixed_0.05_Y$set <- data1$set
      Fixed_0.10_Y$set <- data2$set
    }
    if (n == 2) {
      Fixed_0.05_Y <- data1
      Fixed_0.10_Y <- data2
    }
    png(
      filename = paste0(
        wd,
        fun,
        "eafplot_point",
        prefix1,
        posfix1,
        "_",
        prefix2,
        posfix2,
        ".png"
      ),
      width = 960
    )
    # png(filename = "~/Desktop/eafplot_point.png", width = 960)
    eafplot(
      list(Fixed_0.05 = Fixed_0.05_Y, Fixed_0.10 = Fixed_0.10_Y),
      type = "point",
      main = paste0(name1, "and", name2, " - PF Distribution"),
      legend.pos = "bottomleft"
    )
    dev.off()
    png(
      filename = paste0(
        wd,
        fun,
        "eafplot_area",
        prefix1,
        posfix1,
        "_",
        prefix2,
        posfix2,
        ".png"
      ),
      width = 960
    )
    # png(filename = "~/Desktop/eafplot_area.png", width = 960)
    eafplot(
      list(Fixed_0.05 = Fixed_0.05_Y, Fixed_0.10 = Fixed_0.10_Y),
      type = "area",
      main = paste0(name1, "and", name2, " - Area Covered"),
      legend.pos = "bottomleft"
    )
    dev.off()
    png(
      filename = paste0(
        wd,
        fun,
        "eafdiffplot_area",
        prefix1,
        posfix1,
        "_",
        prefix2,
        posfix2,
        ".png"
      ),
      width = 960
    )
    eafdiffplot(
      data.left = Fixed_0.05_Y,
      data.right = Fixed_0.10_Y,
      type = "area",
      title.left = paste0(name1, "and", name2, "Difference - Points Covered"),
      title.right = paste0(name2, "and", name1, "Difference - Points Covered")
    )
    dev.off()
    png(
      filename = paste0(
        wd,
        fun,
        "eafdiffplot_point",
        prefix1,
        posfix1,
        "_",
        prefix2,
        posfix2,
        ".png"
      ),
      width = 960
    )
    # png(filename = "~/Desktop/eafdiffplot_point.png", width = 960)
    eafdiffplot(
      data.left = Fixed_0.05_Y,
      data.right = Fixed_0.10_Y,
      type = "point",
      title.left = paste0(name1, "and", name2, "Difference - Points Covered"),
      title.right = paste0(name2, "and", name1, "Difference - Points Covered")
    )
    dev.off()
  }

rgl_init <- function(new.device = FALSE,
                     bg = "white",
                     width = 960,
                     theta = 15,
                     phi = 20) {
  if (new.device | rgl.cur() == 0) {
    rgl.open()
    par3d(windowRect = 500 + c(0, 0, width, width))
    rgl.bg(color = bg)
  }
  rgl.clear(type = c("shapes", "bboxdeco"))
  rgl.viewpoint(theta = theta,
                phi = phi,
                zoom = 0.7)
}



savefile_create_fixs <- function(name1, name2) {
  temp <- strsplit(name1, split = " ")
  prefix1 <- temp[[1]][1]
  posfix1 <- strsplit(temp[[1]][2], split = "%")[[1]][1]
  
  out = list(prefix1 = prefix1, posfix1 = posfix1)
  return (out)
}

number_subs <- function(moea, title) {
  plots.data <- moea$plot.resources
  
  selected_priority_function <-
    plots.data$selected_priority_function
  selected_priority_function <-
    subset(selected_priority_function, selected_priority_function != 0)
  
  neighbors_updated <- plots.data$neighbors_updated
  neighbors_updated <-
    subset(neighbors_updated, neighbors_updated != 0)
  
  a <-
    data.frame(
      number = length(selected_priority_function),
      type = paste0("updated.selected:", length(selected_priority_function))
    )
  
  b <-
    data.frame(
      number = length(neighbors_updated),
      type = paste0("updated.neighbors:", length(neighbors_updated))
    )
  temp <- rbind(a, b)
  plot(
    temp$type,
    temp$number,
    xlab = "Substition type",
    ylab = "number of substitutions",
    ylim = c(0, 250000),
    main = title
  )
  
}

pf_by_neihbors <- function(moea, title, filename, ref1) {
  plots.data <- moea$plot.resources
  plots.data <- subset(plots.data, plots.data$`non-dominated` == 1)
  
  temp <-
    subset(moea$plot.paretofront,
           moea$plot.paretofront$`non-dominated` == 1)
  Y <- data.frame(temp$f1, temp$f2)
  
  plots.data$f1 <- Y[, 1]
  plots.data$f2 <- Y[, 2]
  
  p <-
    ggplot(plots.data, aes(f1, f2)) + ggtitle(paste(plots.data$stage, title)) + geom_point(aes(
      color = as.factor(neighbors_updated),
      shape = as.factor(neighbors_updated),
      # size = neighbors_updated,
      alpha = 0.5
    )) +
    # scale_size(range = c(10, .1), name="Neighbors updated")+
    transition_states(stage) +
    theme_ipsum() +
    scale_fill_viridis(discrete = TRUE,
                       guide = FALSE,
                       option = "A")
  anim_save(filename, animation = animate(p, nframes = as.numeric(moea$n.iter)))
}

ed_plot <- function() {
  plot_ly(
    plots.data,
    x = ~ f1,
    y = ~ f2,
    z = ~ f3,
    color = ~ Strategy
  )
  p
}
