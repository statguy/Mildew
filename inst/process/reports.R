library(Mildew)

if (!exists("basePath") | !exists("runParallel"))
  stop("Please set basePath and runParallel parameters.")

library(Mildew)
basePath <- "~/phd/mildew/data"
mildewResults <- MildewResults$new(basePath=basePath)$addAllResults()
mildewResults$summary()
mildewResults$plotYearEstimates(save=T)
mildewResults$plotObservedPredicted(save=T)
mildewResults$plotFixedRandom(save=T)
mildewResults$plotPosteriorRange(save=T)

exclude.imputation.columns <- c("ID","rownames","Commune","PA","Col","Ext","logfallPLM2","Distance_to_shore","S","Smildew","Smildew_pers","y")
mildewResults$results[["ST"]]$occ$getMissingDataProportion(exclude.imputation.columns)
