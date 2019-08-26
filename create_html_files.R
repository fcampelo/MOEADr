rm(list = ls(all = TRUE))
setwd("~/MOEADr/R/")
# library(smoof)
library(MOEADr)
# library(emoa)
library(feather)
# library(withr)
lapply(list.files(pattern = "[.]R$", recursive = TRUE), source)
number.fun <- 7

fun.names1 <- list()
for (i in 1:1) {
    fun.names1[[length(fun.names1) + 1]] = paste0("DTLZ", i)
}
# 
results <- read_feather("~/MOEADr/R/results_dtlz")
# ####
for (fun in fun.names1) {
    ref1 <- read_feather(paste0("~/MOEADr/R/ref1_", fun))

    # agg.igd <- aggregate(results$igd, median, by = list(results$name, results$fun))
    temp.results <- results[results$fun == fun, ]
    # agg.hv <- aggregate(temp.results$hv, median, by = list(temp.results$name, temp.results$fun))
    temp <- subset(temp.results, subset = temp.results$name == "de.data")
    temp <- temp[1:21,]
    id.de <-
        which(temp$igd == median(temp$igd))

    temp <- subset(temp.results, subset = temp.results$name == "norm.data")
    temp <- temp[1:21,]
    id.norm <- which(temp$igd == median(temp$igd))

    temp <- subset(temp.results, subset = temp.results$name == "norm.inverse.data")
    temp <- temp[1:21,]
    id.norm.inverse <- which(temp$igd == median(temp$igd))

    temp <- subset(temp.results, subset = temp.results$name == "R.I.data")
    temp <- temp[1:21,]
    id.R.I. <- which(temp$igd == median(temp$igd))

    temp <- subset(temp.results, subset = temp.results$name == "random.data")
    temp <- temp[1:21,]
    id.random <- which(temp$igd == median(temp$igd))
    print("id.de, id.norm, id.norm.inverse, id.R.I., id.random")
    cat(fun, id.de, id.norm, id.norm.inverse, id.R.I., id.random)

    moead.de <- loadPlotData(name = paste0(fun, "moead.de"), j = id.de)
    moead.norm <- loadPlotData(name = paste0(fun, "moead.norm"), j = id.norm)
    moead.norm.inverse <-
        loadPlotData(name = paste0(fun, "moead.norm.inverse"), j = id.norm.inverse)
    moead.random <-
        loadPlotData(name = paste0(fun, "moead.random"), j = id.random)
    moead.R.I. <-
        loadPlotData(name = paste0(fun, "moead.gra"), j = id.R.I.)


    # p <- visuEvol(moead.de, "MOEA/D-DE - No Resource Allocation", fun, ref1)
    # file <- paste0("~/MOEADr/dataExp/visu/", fun, "_visual_", "MOEAD_DE_None", ".html")
    # saveWidgetFix(p, file)
    #
    # p <- visuEvol(moead.norm, "MOEA/D-DE - Norm Resource Allocation", fun, ref1)
    # file <- paste0("~/MOEADr/dataExp/visu/", fun, "_visual_", "MOEAD_DE_Norm", ".html")
    # saveWidgetFix(p, file)
    #
    # p <- visuEvol(moead.norm.inverse, "MOEA/D-DE - (1-Norm) Resource Allocation", fun, ref1)
    # file <- paste0("~/MOEADr/dataExp/visu/", fun, "_visual_", "MOEAD_DE_InverseNorm", ".html")
    # saveWidgetFix(p, file)
    #
    # p <- visuEvol(moead.random, "MOEA/D-DE - Random Resource Allocation", fun, ref1)
    # file <- paste0("~/MOEADr/dataExp/visu/", fun, "_visual_", "MOEAD_DE_Random", ".html")
    # saveWidgetFix(p, file)
    #
    # p <- visuEvol(moead.R.I., "MOEA/D-DE - R.I. Resource Allocation", fun, ref1)
    # file <- paste0("~/MOEADr/dataExp/visu/", fun, "_visual_", "MOEAD_DE_RI", ".html")
    # saveWidgetFix(p, file)

# }
# # 
# # 
# fun.names1 <- list()
# for (i in 7:number.fun) {
#     fun.names1[[length(fun.names1) + 1]] = paste0("UF", i)
# }
# # 
# results <- read_feather("~/MOEADr/R/results_uf2")
# ####
# for (fun in fun.names1) {
#     ref1 <- read_feather(paste0("~/MOEADr/R/ref1_", fun))
# 
#         temp.results <- results[results$fun == fun, ]
#         # agg.hv <- aggregate(temp.results$hv, median, by = list(temp.results$name, temp.results$fun))
#         temp <- subset(temp.results, subset = temp.results$name == "de.data")
#         temp <- temp[1:21,]
#         id.de <-
#             which(temp$igd == median(temp$igd))
# 
#         temp <- subset(temp.results, subset = temp.results$name == "norm.data")
#         temp <- temp[1:21,]
#         id.norm <- which(temp$igd == median(temp$igd))
# 
#         temp <- subset(temp.results, subset = temp.results$name == "norm.inverse.data")
#         temp <- temp[1:21,]
#         id.norm.inverse <- which(temp$igd == median(temp$igd))
# 
#         temp <- subset(temp.results, subset = temp.results$name == "R.I.data")
#         temp <- temp[1:21,]
#         id.R.I. <- which(temp$igd == median(temp$igd))
# 
#         temp <- subset(temp.results, subset = temp.results$name == "random.data")
#         temp <- temp[1:21,]
#         id.random <- which(temp$igd == median(temp$igd))
#         print("id.de, id.norm, id.norm.inverse, id.R.I., id.random")
#         cat(fun, id.de, id.norm, id.norm.inverse, id.R.I., id.random)
# 
#     moead.de <- loadPlotData(name = paste0(fun, "moead.de"), j = id.de)
#     moead.norm <- loadPlotData(name = paste0(fun, "moead.norm"), j = id.norm)
#     moead.norm.inverse <-
#         loadPlotData(name = paste0(fun, "moead.norm.inverse"), j = id.norm.inverse)
#     moead.random <-
#         loadPlotData(name = paste0(fun, "moead.random"), j = id.random)
#     moead.R.I. <-
#         loadPlotData(name = paste0(fun, "moead.gra"), j = id.R.I.)


    # p <- visuEvol(moead.de, "MOEA/D-DE - No Resource Allocation", fun, ref1)
    # file <- paste0("~/MOEADr/dataExp/visu/", fun, "_visual_", "MOEAD_DE_None", ".html")
    # saveWidgetFix(p, file, background = "lightgray")
    # 
    # p <- visuEvol(moead.norm, "MOEA/D-DE - Norm Resource Allocation", fun, ref1)
    # file <- paste0("~/MOEADr/dataExp/visu/", fun, "_visual_", "MOEAD_DE_Norm", ".html")
    # saveWidgetFix(p, file, background = "lightgray")
    # 
    # p <- visuEvol(moead.norm.inverse, "MOEA/D-DE - (1-Norm) Resource Allocation", fun, ref1)
    # file <- paste0("~/MOEADr/dataExp/visu/", fun, "_visual_", "MOEAD_DE_InverseNorm", ".html")
    # saveWidgetFix(p, file, background = "lightgray")
    # 
    # p <- visuEvol(moead.random, "MOEA/D-DE - Random Resource Allocation", fun, ref1)
    # file <- paste0("~/MOEADr/dataExp/visu/", fun, "_visual_", "MOEAD_DE_Random", ".html")
    # saveWidgetFix(p, file, background = "lightgray")
    # 
    # p <- visuEvol(moead.R.I., "MOEA/D-DE - R.I. Resource Allocation", fun, ref1)
    # file <- paste0("~/MOEADr/dataExp/visu/", fun, "_visual_", "MOEAD_DE_RI", ".html")
    # saveWidgetFix(p, file, background = "lightgray")

}
