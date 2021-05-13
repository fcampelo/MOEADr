rm(list = ls(all = TRUE))

suppressPackageStartupMessages(library(smoof))
suppressPackageStartupMessages(library(MOEADps))
suppressPackageStartupMessages(library(CAISEr))
suppressPackageStartupMessages(library(feather))
suppressPackageStartupMessages(library(car))
suppressPackageStartupMessages(library(ggplot2))
library(R.matlab)

source('~/MOEADr/R/linPF.R')
source("~/MOEADr/R/utils.R")
source('~/MOEADr/R/moead.R')
source('~/MOEADr/R/linPF_ParetoFronts.R')
source('~/MOEADr/R/perform_variation.R')

source('~/MOEADr/R/variation_diffmut.R')

### Build function names 
fname   <- paste0("LINPF", c(3,6))
dims    <- c(12)
allfuns <- expand.grid(fname, dims, stringsAsFactors = FALSE)
fname   <- paste0("MMF", 1:13)
allfuns <- rbind(allfuns,expand.grid(fname, 2, stringsAsFactors = FALSE))
# fname   <- paste0("MMF", 14:1)
# allfuns <- rbind(allfuns,expand.grid(fname, 3, stringsAsFactors = FALSE))



# Assemble instances list
instances <- vector(nrow(allfuns), mode = "list")
for (i in 1:length(instances)) {
  instances[[i]]$FUN <- paste0(allfuns[i, 1], "_", allfuns[i, 2])
  instances[[i]]$number <- i
}

### Build the functions listed above (so that they can be properly used)
for (i in 1:nrow(allfuns)) {
  prob.name <- strsplit(instances[[i]]$FUN, "[0-9]")[[1]][1]
  number <- strsplit(instances[[i]]$FUN, "[A-Z]")[[1]]
  number <- as.numeric(strsplit(number[4], "[_]")[[1]][1])
  prob = strsplit(instances[[i]]$FUN, "[_]")[[1]][1]
  
  if(prob.name == "LINPF"){
    f <- get(paste0("make",prob,"Function"))
    assign(
      x = instances[[i]]$FUN,
      value = function(X) {
        t(apply(X, MARGIN = 1,
                FUN = f()))
      }
    )
  }
  else if(prob.name == "SYMPARTrotated_" || prob.name == "SYMPARTsimple_" || prob.name ==  "OmniTest_"){
    f <- get(paste0("make",prob,"Function"))
    assign(
      x = instances[[i]]$FUN,
      value = function(X) {
        t(apply(X, MARGIN = 1,
                FUN = f()))
      }
    )
  }
  else{
    if(number < 14){
      assign(
        x = instances[[i]]$FUN,
        value = MOEADr::make_vectorized_smoof(
          prob.name  = prob
        ))
    }
    else{
      assign(
        x = instances[[i]]$FUN,
        value = MOEADr::make_vectorized_smoof(
          prob.name  = prob, dimensions = 3L, n.objectives = 3L, np = 2L
        ))
    }
    
  }
  
}

# Prepare algorithm function to be used in run_experiment():
myalgo <- function(type, instance) {
  # Input parameters:
  #     - type (variant to use: "original", "original2", "moead.de" or "moead.de2")
  #     - instance (instance to be solved, e.g., instance = instances[[i]])
  # All other parameters are set internally
  
  ## Extract instance information to build the MOEADr problem format
  fdef  <- unlist(strsplit(instance$FUN, split = "_"))
  prob.name <- strsplit(fdef, "[0-9]")[[1]][1]
  prob <- strsplit(fdef, "[_]")[[1]][1]
  
  f <- get(paste0("make",prob,"Function"))
  uffun <- f()  
  
  fattr    <- attr(uffun, "par.set")
  prob.dim <- fattr$pars$x$len
  n.obj <- attr(uffun, "n.objectives")
  
  ## Build MOEADr problem list
  problem <- list(
    name = instance$FUN,
    xmin = fattr$pars$x$lower,
    xmax = fattr$pars$x$upper,
    m    = n.obj
  )
  
  
  
  
  ## Load presets for the algorithm provided in input 'type' and
  ## modify whatever is needed for this particular experiment
  algo.preset <- preset_moead("moead.de")
  if (n.obj == 2) {
    loaded.weights.2objs <-
      data.matrix(
        read.csv(
          "~/MOEADr/SOBOL-2objs-500wei.ws",
          header = F,
          stringsAsFactors = FALSE,
          sep = " "
        )
      )
    decomp <- list(name = "loaded", W = loaded.weights.2objs)
    algo.preset$neighbors$T <- 350*0.2
  }
  else{
    loaded.weights.3objs <-
      data.matrix(
        read.csv(
          "~/MOEADr/SOBOL-3objs-500wei.ws",
          header = F,
          stringsAsFactors = FALSE,
          sep = " "
        )
      )
    decomp <- list(name = "loaded", W = loaded.weights.3objs)
    algo.preset$neighbors$T <- floor(500*0.2)
  }
  
  
  algo.preset$stopcrit[[1]]$name <-
    "maxeval" # <-- type of stop criterion
  algo.preset$stopcrit[[1]]$maxeval <- 30000 # stop crit.
  poly.ind <- which(sapply(algo.preset$variation,
                           function(x) {
                             x$name == "polymut"
                           }))
  algo.preset$variation[[poly.ind]]$pm <-
    1 / prob.dim # <--- pm = 1/d
  algo.preset$UseArchive = TRUE
  algo.preset$scaling <- list(name = "simple")
  
  ## Run algorithm on "instance"
  if (type == "moead.ps") {
    
    resource.allocation <-
      list(
        name = "random",
        dt = 0,
        selection = "n",
        n = algo.preset$neighbors$T/2,
        period = - 1
      )
    out <-
      moeadps(
        preset = algo.preset,
        problem = problem,
        decomp = decomp,
        resource.allocation = resource.allocation,
        showpars = list(show.iters = "none")
      )
    
  }
  else if (type == "moead.ps2") {
    
    resource.allocation <-
      list(
        name = "random",
        dt = 0,
        selection = "n",
        n = algo.preset$neighbors$T/2,
        period = 0
      )
    out <-
      moeadps(
        preset = algo.preset,
        problem = problem,
        decomp = decomp,
        resource.allocation = resource.allocation,
        showpars = list(show.iters = "none")
      )
    
  } else{
    out <-
      moeadps(
        preset = algo.preset,
        problem = problem,
        decomp = decomp,
        showpars = list(show.iters = "none")
      )
  }
  # foldername <- paste0(
  #   "~/Desktop/",
  #   instance$number,
  #   "_",
  #   instance$FUN,
  #   "_",
  #   type
  # )
  # if(!dir.exists(foldername)){
  #   dir.create(foldername)  
  # }
  # i <- 0
  # while(TRUE){
  #   tmp <- paste0(
  #     foldername,
  #     "/PS_",
  #     type,"_",i,".txt"
  #   )
  #   if (!file.exists(tmp)){
  #     write.csv(out$PF, file = tmp)
  #     break
  #   }
  #   else{
  #     i <-i + 1
  #   }
  # }
  
  
  if(prob.name == "MMF"){
    pspf <- readMat(paste0("~/Downloads/MMO/MMO test problems/Reference_PSPF_data/",fdef[1],"_Reference_PSPF_data.mat"))
    pf <- pspf$PF
    pf <- (as.data.frame(pf))
    colnames(pf) <- c("f1", "f2")  
  }
  else{
    if(prob == "LINPF1") pf <- linPF1()
    else if(prob == "LINPF2") pf <- linPF2()
    else if(prob == "LINPF3") pf <- linPF3()
    else if(prob == "LINPF4") pf <- linPF4()
    else if(prob == "LINPF5") pf <- linPF5()
    else if(prob == "LINPF6") pf <- linPF6()
    else if(prob.name == "SYMPARTrotated"){
      pspf <- readMat(paste0("~/Downloads/MMO/MMO test problems/Reference_PSPF_data/SYM_PART_rotated_Reference_PSPF_data.mat"))
      pf <- pspf$PF
      pf <- (as.data.frame(pf))
      colnames(pf) <- c("f1", "f2")  
    }
    else if(prob.name == "SYMPARTsimple"){
      pspf <- readMat(paste0("~/Downloads/MMO/MMO test problems/Reference_PSPF_data/SYM_PART_simple_Reference_PSPF_data.mat"))
      pf <- pspf$PF
      pf <- (as.data.frame(pf))
      colnames(pf) <- c("f1", "f2")  
    }
    else if(prob.name == "OmniTest"){
      pspf <- readMat(paste0("~/Downloads/MMO/MMO test problems/Reference_PSPF_data/Omni_test_Reference_PSPF_data.mat"))
      pf <- pspf$PF
      pf <- (as.data.frame(pf))
      colnames(pf) <- c("f1", "f2")  
    }
  }
  
  
  IGD = calcIGD(Y = out$Y, Yref = pf)
  ## Return IGD as field "value" in the output list
  return(list(value = IGD))
}

algorithms <- list(
  list(FUN   = "myalgo",
       alias = "MOEAD.DE",
       type  = "moead.de"),
  list(FUN   = "myalgo",
       alias = "MOEAD.PS",
       type  = "moead.ps")
)

my.results <- run_experiment(
  instances  = instances,
  algorithms = algorithms,
  power = 0.8,
  # Desired power: 80%
  power.target = "mean",
  # on average,
  d = 0.5,
  # to detect differences greater
  # than 0.5 standard deviations
  sig.level = 0.05,
  # at a 95% confidence level.
  se.max = 0.05,
  # Measurement error: 5%
  dif = "perc",
  test = "wilcoxon",
  # on the paired percent
  # differences of means,
  method = "param",
  # calculated using parametric
  # formula.
  comparisons = "all.vs.all",
  # Compare all algorithms
  # vs all others,
  nstart = 15,
  # start with 15 runs/algo/inst2
  nmax   = 400,
  # and do no more than 200 runs/inst.
  # PRNG seed (for reproducibility)
  #
  # NOTICE: Using all but 1 cores. Change if needed
  ncpus  = 3,
  save.partial.results = "./"
)

plot(my.results)

  


algopairs <- paste(my.results$data.summary$Alg1,
                   my.results$data.summary$Alg2,
                   sep = " - ")


par(mfrow = c(1, 1))
df <- cbind(Comparison = algopairs, my.results$data.summary)

mp <- ggplot(df, aes(x = Comparison, y = Phi, fill = Comparison))
mp +
  geom_violin(alpha = 0.6,
              show.legend = FALSE,
              scale = "width") +
  geom_boxplot(
    alpha = 0,
    show.legend = FALSE,
    outlier.shape = NA,
    width = .15
  ) +
  geom_point(
    shape = 16,
    col = "black",
    fill = "black",
    alpha = 0.6,
    position = position_jitter(width = .15)
  ) +
  geom_abline(
    slope = 0,
    intercept = 0,
    col = "red",
    lty = 2
  ) +
  ylab("Percent difference in IGD") + xlab("") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme_minimal()

summary(my.results, test = "wilcoxon")

print(ggplot(df, aes(x = Instance, y = Phi, colour = Comparison,
                     ymin = Phi - SE, ymax = Phi + SE)) + 
        geom_pointrange(show.legend = FALSE) + 
        geom_abline(slope = 0, intercept = 0, col = 1, lty = 2) + 
        facet_grid(Comparison ~ .) + 
        theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
        xlab("") + theme_minimal()
)

# linPF_CEC_results.rds <- my.results
# 
# save(uf_results.rds, "UF_results.rds")

