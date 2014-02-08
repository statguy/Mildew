library(Mildew)

basePath <- "~/phd/mildew/data" # Set your path to the data files here

occ <- OccupancyMildew$new(basePath=basePath)
col <- ColonizationMildew$new(basePath=basePath)
ext <- ExtinctionMildew$new(basePath=basePath)

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
