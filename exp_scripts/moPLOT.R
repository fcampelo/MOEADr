library(moPLOT)
library(fields)
## runDashboard()


fn = smoof::makeBiObjBBOBFunction(dimensions = 2, fid = 35, iid = 1)
problem.DTLZ <- function(X) {
  t(apply(X, MARGIN = 1,
           FUN = fn))
}


# Generate a design in the (rectangular) decision space of fn
design = moPLOT::generateDesign(fn, 500**2)
# design$obj.space = calculateObjectiveValues(design$dec.space, fn, parallelize = T)
design$obj.space = problem.DTLZ(design$dec.space)
colnames(design$obj.space) <- c("y1", "y2")

# Calculate single-objective and multi-objective gradients
gradients = computeGradientFieldGrid(design)

# Calculate divergence of MOG
divergence = computeDivergenceGrid(gradients$multi.objective, design$dims, design$step.sizes)

# Calculate locally efficient points
less = localEfficientSetSkeleton(design, gradients, divergence, integration="fast")

# Plot the results
# ggplotPLOT(design$dec.space, design$obj.space, less$sinks, less$height)
# moPLOT::ggplotPLOTObjSpace(design$obj.space, less$sinks, less$height)

ggplotPLOT <- function (dec.space, obj.space, sinks, height, check.data = TRUE) 
{
  if (check.data) {
    assertMatrix(dec.space, ncols = 2, col.names = "unique")
    assertMatrix(obj.space, min.cols = 2, nrows = nrow(dec.space))
    assertInteger(sinks, lower = 1, upper = nrow(dec.space))
    assertNumeric(height, len = nrow(dec.space))
  }
  nds = ecr::doNondominatedSorting(t(obj.space[sinks, ]))
  height[height == 0] = min(height[height > 0])/2
  height.df = cbind.data.frame(dec.space, height = height)
  sinks.df = cbind.data.frame(dec.space[sinks, ], rank = nds$dom.counter + 
                                1)
  sinks.df = sinks.df[order(sinks.df$rank, decreasing = T), 
                      ]
  var1 = colnames(dec.space)[1]
  var2 = colnames(dec.space)[2]
  colorscale.efficient = fields::tim.colors(500L)
  colorscale.heatmap = gray.colors(500L, start = 0, end = 1, 
                                   gamma = 0.5)
  if (all(sinks.df$rank == sinks.df$rank[1])) {
    colorscale.efficient = colorscale.efficient[1]
  }
  g = ggplotHeatmap(height.df, color.palette = colorscale.heatmap, 
                    var1 = var1, var2 = var2) + geom_point(mapping = aes_string(var1, 
                                                                                var2, color = "rank"), data = sinks.df) + scale_color_gradientn(colors = heat.colors(500L), 
                                                                                                                                                na.value = "transparent", trans = "log") + theme(legend.position = "none")
  return(g)
}

ggplotPLOTObjSpace <- function (obj.space, sinks, height, check.data = TRUE) {
  if (check.data) {
    assertMatrix(obj.space, ncols = 2)
    assertInteger(sinks, lower = 1, upper = nrow(obj.space))
    assertNumeric(height, len = nrow(obj.space))
  }
  nds = ecr::doNondominatedSorting(t(obj.space[sinks, ]))
  height[height == 0] = min(height[height > 0])/2
  height.obj.df = cbind.data.frame(obj.space, height = height)
  height.obj.df = height.obj.df[order(height.obj.df$height, 
                                      decreasing = T), ]
  sinks.obj.df = cbind.data.frame(obj.space[sinks, ], height = nds$dom.counter + 
                                    1)
  sinks.obj.df = sinks.obj.df[order(sinks.obj.df$height, decreasing = T), 
                              ]
  var1 = colnames(obj.space)[1]
  var2 = colnames(obj.space)[2]
  g = ggplot() + geom_point(mapping = aes_string(var1, var2, 
                                                 color = "height"), data = height.obj.df) + geom_point(mapping = aes_string(var1, 
                                                                                                                            var2, fill = "height", color = NA), data = sinks.obj.df, 
                                                                                                       shape = 21) + scale_color_gradientn(colors = gray.colors(500L, 
                                                                                                                                                                start = 0, end = 1, gamma = 0.5), na.value = "transparent", 
                                                                                                                                           trans = "log") + scale_fill_gradientn(colors = heat.colors(500L), 
                                                                                                                                                                                 trans = "log") + 
     theme_minimal() + 
    theme(legend.position = "none")
  return(g)
}

ggplotPLOT(design$dec.space, design$obj.space, less$sinks, less$height)
ggplotPLOTObjSpace(design$obj.space, less$sinks, less$height)
