rm(list = ls(all = TRUE))
library(smoof)
library(emoa)
library(feather)
library(compiler)
library(ggplot2)
library(ggthemes)
library(MOEADps)
library(eaf)
options(kableExtra.latex.load_packages = T)
library(kableExtra)
options(scipen = 0)
# options(digits = 3)
library("CAISEr")

source("~/MOEADr/R/utils.R")


stats0 <- read_feather("~/tec/ecj_stats")




strategy <-
  c("MOEA/D-PS",
    "Big pop.",
    "Small pop.")

for (iter in unique(stats0$iter)) {
  # if (as.numeric(iter) >= 20000) {
  #   break
  # } else{
    p.values <- data.frame()
    Est <- data.frame()
    CIs <- data.frame()
    Comps <- list()
  # }
  for (name in strategy) {
    # if(name != "MOEA/D-PS"){
      
    
    stats1 <- stats0[stats0$iter == iter, ]
    stats1 <- stats1[stats1$Strategy != name,]
    stats2 <-
      aggregate(stats1$hv, median, by = list(stats1$Strategy, stats1$fun))
    colnames(stats2) <- c("Strategy", "fun", "hv")
    
    tmp <- wilcox.test(
      stats2$hv ~
        stats2$Strategy,
      paired = TRUE,
      p.adj = "hommel",
      conf.int = TRUE
    )
    
    
      # p.values <- rbind(p.values, c(tmp, as.numeric(iter)))
    p.values <- rbind(p.values, round(c(tmp$p.value),3))
    Est <- rbind(Est, c(tmp$estimate))
    CIs <- rbind(CIs, round(tmp$conf.int,3))
    Comps[[length(Comps) + 1]] <-
      paste(strategy[strategy != name][1], " x ", strategy[strategy != name][2])
    # }
  }
  
  Comps <-  data.frame(unlist(Comps))
  
  df <- data.frame(
    Comparison = Comps,
    Estimate   = Est,
    CIl        = CIs[, 1],
    CIu        = CIs[, 2],
    p.value    = p.values,
    alpha      = 0.05,
    stringsAsFactors = FALSE
  )
  
  colnames(df) <-
    c("Comparison", "Estimate", "CIl",  "CIu", "p.value", "alpha")
  df$Reject <- df$p.value <= df$alpha
  
  pvaltxt  <- paste0("p = ", df$p.value)
  alphatxt <- paste0("alpha = ", df$alpha)
  CItxt    <- paste0("$CI = [", df$CIl, ", ",
                     df$CIu, "]")
  ylabtxt  <- "Est. Difference"
  
  
  df <- cbind(df, pvaltxt, alphatxt, CItxt)
  
  mp <- ggplot2::ggplot(
    df,
    ggplot2::aes_string(
      x = "Comparison",
      y = "Estimate",
      ymin = "CIl",
      ymax = "CIu",
      colour = "!Reject",
      shape  = "Reject"
    )
  ) +
    ggplot2::theme_minimal(base_size = 20) +
    ggplot2::geom_abline(
      slope = 0,
      intercept = 0,
      lty = 3,
      lwd = 1.4,
      alpha = .5
    ) +
    ggplot2::geom_pointrange(size = 1.1,
                             fatten = 2,
                             
                             show.legend = FALSE) +
    ggplot2::ylab("Est. Difference") + ggplot2::xlab("") +
    ggplot2::scale_shape_manual(values = c(16, 18)) +
    ggplot2::coord_flip()
  
  mp <- mp +
    ggplot2::geom_text(
      ggplot2::aes(label = CItxt),
      nudge_x = .2,
      size = 5,
      col = 1
    ) +
   ggplot2::geom_text(
      ggplot2::aes(label = alphatxt),
      nudge_x = -.175,
      size = 5,
      col = 1
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = pvaltxt),
      nudge_x = -.35,
      size = 5,
      col = 1
    )
  
  
  print(mp)
 
  filename = paste0("~/Desktop/conf_int_", iter , ".png")
  ggsave(
    filename = filename,
    dpi = 200,
    width = 12,
    height = 4
  )
}
