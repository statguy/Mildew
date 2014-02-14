library(Mildew)

if (!exists("basePath") | !exists("runParallel"))
  stop("Please set basePath and runParallel parameters.")

occ <- OccupancyMildew$new(basePath=basePath)
col <- ColonizationMildew$new(basePath=basePath)
ext <- ExtinctionMildew$new(basePath=basePath)

exclude.imputation.columns <- c("ID","rownames","Commune","PA","Col","Ext","logfallPLM2","Distance_to_shore","S","Smildew","Smildew_pers","y")
occ$getMissingDataProportion(exclude.imputation.columns)

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



mildewResults <- MildewResults$new(basePath=basePath)
mildewResults$plotYearEstimates(save=T)
mildewResults$plotObservedPredicted(save=T)
