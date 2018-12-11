  source("~/MOEADr/graphical_analysis.R")
  setwd("~/MOEADr/")
  ref.points<- rep(1+1/350,2)
  repetitions <- 30
  gens <- 200
  
  fun.names<-list()
  for (i in 1:6) {
    fun.names[[length(fun.names) + 1]] = paste0("UF", i)
    # fun.names[[length(fun.names) + 1]] = paste0("DTLZ", i)
  }
  
  max.val <- c(-Inf, -Inf)
  min.val <- c(Inf, Inf)
  id <- 1
  for (fun in fun.names){
    # Yref <- as.matrix(read.table(paste0("inst/extdata/pf_data/UF",id, ".dat")))
    # Yref <- as.matrix(read.table(paste0("inst/extdata/pf_data/DTLZ",id, ".2D.pf")))
    Yref <- as.matrix(read.table(paste0("inst/extdata/pf_data/DTLZ",id, ".2D.pf")))
    id <- id + 1
    for (i in 1:repetitions){
      for (gen in 1:gens){
        pdGen <- formatC(gen-1, width = 3, format = "d", flag = "0")
        my.file.n <- paste0("../de/", fun,"_rep_",i,"_",pdGen,"_Y")
        data <- as.matrix(read_feather(my.file.n))
        max.val <- pmax(max.val, apply(data, 2, max))
        min.val <- pmin(min.val, apply(data, 2, min))
      }
    }
    
    for (i in 1:repetitions){
      for (gen in 1:gens){
        pdGen <- formatC(gen-1, width = 3, format = "d", flag = "0")
        my.file.n <- paste0("../rad/", fun,"_rep_",i,"_",pdGen,"_Y")
        data <- as.matrix(read_feather(my.file.n))
        max.val <- pmax(max.val, apply(data, 2, max))
        min.val <- pmin(min.val, apply(data, 2, min))
      }
    }
    
    for (i in 1:repetitions){
      for (gen in 1:gens){
        pdGen <- formatC(gen-1, width = 3, format = "d", flag = "0")
        my.file.n <- paste0("../dra/", fun,"_rep_",i,"_",pdGen,"_Y")
        data <- as.matrix(read_feather(my.file.n))
        max.val <- pmax(max.val, apply(data, 2, max))
        min.val <- pmin(min.val, apply(data, 2, min))
      }
    }
    for (i in 1:repetitions){
      for (gen in 1:gens){
        pdGen <- formatC(gen-1, width = 3, format = "d", flag = "0")
        my.file.n <- paste0("../gra/", fun,"_rep_",i,"_",pdGen,"_Y")
        data <- as.matrix(read_feather(my.file.n))
        max.val <- pmax(max.val, apply(data, 2, max))
        min.val <- pmin(min.val, apply(data, 2, min))
      }
    }
    
    for (i in 1:repetitions){
      for (gen in 1:gens){
        pdGen <- formatC(gen-1, width = 3, format = "d", flag = "0")
        my.file.n <- paste0("../norm/", fun,"_rep_",i,"_",pdGen,"_Y")
        data <- as.matrix(read_feather(my.file.n))
        max.val <- pmax(max.val, apply(data, 2, max))
        min.val <- pmin(min.val, apply(data, 2, min))
      }
    }
    
    for (i in 1:repetitions){
      for (gen in 1:gens){
        pdGen <- formatC(gen-1, width = 3, format = "d", flag = "0")
        my.file.n <- paste0("../random/", fun,"_rep_",i,"_",pdGen,"_Y")
        data <- as.matrix(read_feather(my.file.n))
        max.val <- pmax(max.val, apply(data, 2, max))
        min.val <- pmin(min.val, apply(data, 2, min))
      }
    }
  }
  
  
  my.data <- data.frame()
  for (fun in fun.names){
    
    
    median_gen <- data.frame()
    for (repetition in 1:repetitions){
      hvs <- data.frame()
      igds <- data.frame()
      hv_gen <- data.frame()  
      igd_gen <- data.frame()  
      for (gen in 1:gens){
        pdGen <- formatC(gen-1, width = 3, format = "d", flag = "0")
        my.file.n <- paste0("../de/", fun,"_rep_",repetition,"_",pdGen,"_Y")
        data <- as.matrix(read_feather(my.file.n))
        hvs <- rbind(hvs, calc_hv(data, "de", fun, gen, max.val, min.val, ref.points))
        igds <- rbind(igds, calcIGD(data, Yref))
      }
      hvs <- hvs[,c(4,1)]
      names(hvs) <- c("gen", "HV")
      names(igds) <- c("IGD")
      hv_gen <- rbind(hv_gen, hvs)
      igd_gen <- rbind(igd_gen, igds)
      median_gen <- rbind(median_gen, data.frame(hv_gen, igd_gen, repetition))
    }
    median_hv <- aggregate(median_gen$HV, FUN = median, by = list(median_gen$gen))
    sd_hv <- aggregate(median_gen$HV, FUN = sd, by = list(median_gen$gen))
    median_igd <- aggregate(median_gen$IGD, FUN = median, by = list(median_gen$gen))
    sd_igd <- aggregate(median_gen$IGD, FUN = sd, by = list(median_gen$gen))
    median_gen <- cbind(median_hv, median_igd)
    sd_gen <- cbind(median_hv, median_igd)
    median_gen <- median_gen[,c(1,2,4)]
    sd_gen <- sd_gen[,c(2,4)]
    names(median_gen) <- c("gen", "HV", "IGD")  
    names(sd_gen) <- c("sd_HV", "sd_IGD")  
    de_median_gen <- cbind(median_gen, sd_gen)
    
    median_gen <- data.frame()
    for (repetition in 1:repetitions){
      hvs <- data.frame()
      igds <- data.frame()
      hv_gen <- data.frame()  
      igd_gen <- data.frame()  
      for (gen in 1:gens){
        pdGen <- formatC(gen-1, width = 3, format = "d", flag = "0")
        my.file.n <- paste0("../rad/", fun,"_rep_",repetition,"_",pdGen,"_Y")
        data <- as.matrix(read_feather(my.file.n))
        hvs <- rbind(hvs, calc_hv(data, "de", fun, gen, max.val, min.val, ref.points))
        igds <- rbind(igds, calcIGD(data, Yref))
      }
      hvs <- hvs[,c(4,1)]
      names(hvs) <- c("gen", "HV")
      names(igds) <- c("IGD")
      hv_gen <- rbind(hv_gen, hvs)
      igd_gen <- rbind(igd_gen, igds)
      median_gen <- rbind(median_gen, data.frame(hv_gen, igd_gen, repetition))
    }
    median_hv <- aggregate(median_gen$HV, FUN = median, by = list(median_gen$gen))
    sd_hv <- aggregate(median_gen$HV, FUN = sd, by = list(median_gen$gen))
    median_igd <- aggregate(median_gen$IGD, FUN = median, by = list(median_gen$gen))
    sd_igd <- aggregate(median_gen$IGD, FUN = sd, by = list(median_gen$gen))
    median_gen <- cbind(median_hv, median_igd)
    sd_gen <- cbind(median_hv, median_igd)
    median_gen <- median_gen[,c(1,2,4)]
    sd_gen <- sd_gen[,c(2,4)]
    names(median_gen) <- c("gen", "HV", "IGD")  
    names(sd_gen) <- c("sd_HV", "sd_IGD")  
    rad_median_gen <- cbind(median_gen, sd_gen)
    
    median_gen <- data.frame()
    for (repetition in 1:repetitions){
      hvs <- data.frame()
      igds <- data.frame()
      hv_gen <- data.frame()  
      igd_gen <- data.frame()  
      for (gen in 1:gens){
        pdGen <- formatC(gen-1, width = 3, format = "d", flag = "0")
        my.file.n <- paste0("../dra/", fun,"_rep_",repetition,"_",pdGen,"_Y")
        data <- as.matrix(read_feather(my.file.n))
        hvs <- rbind(hvs, calc_hv(data, "de", fun, gen, max.val, min.val, ref.points))
        igds <- rbind(igds, calcIGD(data, Yref))
      }
      hvs <- hvs[,c(4,1)]
      names(hvs) <- c("gen", "HV")
      names(igds) <- c("IGD")
      hv_gen <- rbind(hv_gen, hvs)
      igd_gen <- rbind(igd_gen, igds)
      median_gen <- rbind(median_gen, data.frame(hv_gen, igd_gen, repetition))
    }
    median_hv <- aggregate(median_gen$HV, FUN = median, by = list(median_gen$gen))
    sd_hv <- aggregate(median_gen$HV, FUN = sd, by = list(median_gen$gen))
    median_igd <- aggregate(median_gen$IGD, FUN = median, by = list(median_gen$gen))
    sd_igd <- aggregate(median_gen$IGD, FUN = sd, by = list(median_gen$gen))
    median_gen <- cbind(median_hv, median_igd)
    sd_gen <- cbind(median_hv, median_igd)
    median_gen <- median_gen[,c(1,2,4)]
    sd_gen <- sd_gen[,c(2,4)]
    names(median_gen) <- c("gen", "HV", "IGD")  
    names(sd_gen) <- c("sd_HV", "sd_IGD")  
    dra_median_gen <- cbind(median_gen, sd_gen)
    
    median_gen <- data.frame()
    for (repetition in 1:repetitions){
      hvs <- data.frame()
      igds <- data.frame()
      hv_gen <- data.frame()  
      igd_gen <- data.frame()  
      for (gen in 1:gens){
        pdGen <- formatC(gen-1, width = 3, format = "d", flag = "0")
        my.file.n <- paste0("../gra/", fun,"_rep_",repetition,"_",pdGen,"_Y")
        data <- as.matrix(read_feather(my.file.n))
        hvs <- rbind(hvs, calc_hv(data, "de", fun, gen, max.val, min.val, ref.points))
        igds <- rbind(igds, calcIGD(data, Yref))
      }
      hvs <- hvs[,c(4,1)]
      names(hvs) <- c("gen", "HV")
      names(igds) <- c("IGD")
      hv_gen <- rbind(hv_gen, hvs)
      igd_gen <- rbind(igd_gen, igds)
      median_gen <- rbind(median_gen, data.frame(hv_gen, igd_gen, repetition))
    }
    median_hv <- aggregate(median_gen$HV, FUN = median, by = list(median_gen$gen))
    sd_hv <- aggregate(median_gen$HV, FUN = sd, by = list(median_gen$gen))
    median_igd <- aggregate(median_gen$IGD, FUN = median, by = list(median_gen$gen))
    sd_igd <- aggregate(median_gen$IGD, FUN = sd, by = list(median_gen$gen))
    median_gen <- cbind(median_hv, median_igd)
    sd_gen <- cbind(median_hv, median_igd)
    median_gen <- median_gen[,c(1,2,4)]
    sd_gen <- sd_gen[,c(2,4)]
    names(median_gen) <- c("gen", "HV", "IGD")  
    names(sd_gen) <- c("sd_HV", "sd_IGD")   
    gra_median_gen <- cbind(median_gen, sd_gen)
    
    median_gen <- data.frame()
    for (repetition in 1:repetitions){
      hvs <- data.frame()
      igds <- data.frame()
      hv_gen <- data.frame()  
      igd_gen <- data.frame()  
      for (gen in 1:gens){
        pdGen <- formatC(gen-1, width = 3, format = "d", flag = "0")
        my.file.n <- paste0("../random/", fun,"_rep_",repetition,"_",pdGen,"_Y")
        data <- as.matrix(read_feather(my.file.n))
        hvs <- rbind(hvs, calc_hv(data, "de", fun, gen, max.val, min.val, ref.points))
        igds <- rbind(igds, calcIGD(data, Yref))
      }
      hvs <- hvs[,c(4,1)]
      names(hvs) <- c("gen", "HV")
      names(igds) <- c("IGD")
      hv_gen <- rbind(hv_gen, hvs)
      igd_gen <- rbind(igd_gen, igds)
      median_gen <- rbind(median_gen, data.frame(hv_gen, igd_gen, repetition))
    }
    median_hv <- aggregate(median_gen$HV, FUN = median, by = list(median_gen$gen))
    sd_hv <- aggregate(median_gen$HV, FUN = sd, by = list(median_gen$gen))
    median_igd <- aggregate(median_gen$IGD, FUN = median, by = list(median_gen$gen))
    sd_igd <- aggregate(median_gen$IGD, FUN = sd, by = list(median_gen$gen))
    median_gen <- cbind(median_hv, median_igd)
    sd_gen <- cbind(median_hv, median_igd)
    median_gen <- median_gen[,c(1,2,4)]
    sd_gen <- sd_gen[,c(2,4)]
    names(median_gen) <- c("gen", "HV", "IGD")  
    names(sd_gen) <- c("sd_HV", "sd_IGD")  
    random_median_gen <- cbind(median_gen, sd_gen)
    
    median_gen <- data.frame()
    for (repetition in 1:repetitions){
      hvs <- data.frame()
      igds <- data.frame()
      hv_gen <- data.frame()  
      igd_gen <- data.frame()  
      for (gen in 1:gens){
        pdGen <- formatC(gen-1, width = 3, format = "d", flag = "0")
        my.file.n <- paste0("../norm/", fun,"_rep_",repetition,"_",pdGen,"_Y")
        data <- as.matrix(read_feather(my.file.n))
        hvs <- rbind(hvs, calc_hv(data, "de", fun, gen, max.val, min.val, ref.points))
        igds <- rbind(igds, calcIGD(data, Yref))
      }
      hvs <- hvs[,c(4,1)]
      names(hvs) <- c("gen", "HV")
      names(igds) <- c("IGD")
      hv_gen <- rbind(hv_gen, hvs)
      igd_gen <- rbind(igd_gen, igds)
      median_gen <- rbind(median_gen, data.frame(hv_gen, igd_gen, repetition))
    }
    median_hv <- aggregate(median_gen$HV, FUN = median, by = list(median_gen$gen))
    sd_hv <- aggregate(median_gen$HV, FUN = sd, by = list(median_gen$gen))
    median_igd <- aggregate(median_gen$IGD, FUN = median, by = list(median_gen$gen))
    sd_igd <- aggregate(median_gen$IGD, FUN = sd, by = list(median_gen$gen))
    median_gen <- cbind(median_hv, median_igd)
    sd_gen <- cbind(median_hv, median_igd)
    median_gen <- median_gen[,c(1,2,4)]
    sd_gen <- sd_gen[,c(2,4)]
    names(median_gen) <- c("gen", "HV", "IGD")  
    names(sd_gen) <- c("sd_HV", "sd_IGD")  
    norm_median_gen <- cbind(median_gen, sd_gen)
    
    my.data <- rbind(my.data, de_median_gen[nrow(de_median_gen),], rad_median_gen[nrow(rad_median_gen),], dra_median_gen[nrow(dra_median_gen),], gra_median_gen[nrow(gra_median_gen),], norm_median_gen[nrow(norm_median_gen),], random_median_gen[nrow(random_median_gen),])
    rownames(my.data) <- c("DE", "RAD", "DRA", "GRA", "NORM", "RANDOM")
    
    
    
    
  }
  
  
  de_median_gen$name <- "de"
  rad_median_gen$name <- "rad"
  dra_median_gen$name <- "dra"
  gra_median_gen$name <- "gra"
  random_median_gen$name <- "random"
  norm_median_gen$name <- "norm"
  
  
  df <- rbind(de_median_gen, rad_median_gen, gra_median_gen, random_median_gen, random_median_gen, norm_median_gen)
  
  
  pathname <- "../files/hv_all.png"
  p <- ggplot(df, aes(gen+1, HV, group=name)) +
    geom_line(aes(linetype="dashed", color=name))+
    geom_point(aes(shape=name, color = name))+
    geom_errorbar(aes(ymin=HV-sd_HV, ymax=HV+sd_HV), width=.1) 
  # p
  ggsave(filename = pathname, device = "png")
  
  pathname <- "../files/igd_all.png"
  p <- ggplot(df, aes(gen+1, IGD, group=name)) +
    geom_line(aes(linetype="dashed", color=name))+
    geom_point(aes(shape=name, color = name))+
    geom_errorbar(aes(ymin=IGD-sd_IGD, ymax=IGD+sd_IGD), width=.1) 
  # p
  ggsave(filename = pathname, device = "png")
  
  
  create_graphs(my.data, fun.names, 2)
  