source("classes.R")

basePath <- "~/phd/mildew/data" # Set your path to the data files here
runParallel <- TRUE

cluster <- NA
if (runParallel) {
  library(devtools)
  source_url("https://raw.github.com/statguy/RSnippets/master/Cluster/Cluster.R")
  cluster <- Cluster$new()
  cluster$startLocalCluster()
}


occ <- OccupancyMildew$new(basePath=basePath, runParallel=runParallel)
col <- ColonizationMildew$new(basePath=basePath, runParallel=runParallel)
ext <- ExtinctionMildew$new(basePath=basePath, runParallel=runParallel)

summaryResult <- function(occ, col, ext, type, tag="") {
  tryCatch(occ$loadResult(type, tag)$summaryResult()$summaryHyperparameters(), error=function(e) message("Error: ", e$message))
  tryCatch(col$loadResult(type, tag)$summaryResult()$summaryHyperparameters(), error=function(e) message("Error: ", e$message))
  tryCatch(ext$loadResult(type, tag)$summaryResult()$summaryHyperparameters(), error=function(e) message("Error: ", e$message))
}

summaryResult(occ, col, ext, type="glm")
summaryResult(occ, col, ext, type="spatiotemporal", tag="interceptonly")
summaryResult(occ, col, ext, type="spatialonly")
summaryResult(occ, col, ext, type="temporalreplicate")
summaryResult(occ, col, ext, type="spatialreplicate")
summaryResult(occ, col, ext, type="spatiotemporal")


if (runParallel) {
  cluster$finalize()
}
