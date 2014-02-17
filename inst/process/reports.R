library(Mildew)

if (!exists("basePath") | !exists("runParallel"))
  stop("Please set basePath and runParallel parameters.")

mildewResults <- MildewResults$new(basePath=basePath)$addAllResults()
mildewResults$summary()
mildewResults$plotYearEstimates(save=T)
mildewResults$plotObservedPredicted(save=T)
mildewResults$plotFixedRandom(save=T)
mildewResults$plotPosteriorRange(save=T)

exclude.imputation.columns <- c("ID","rownames","Commune","PA","Col","Ext","logfallPLM2","Distance_to_shore","S","Smildew","Smildew_pers","y")
MildewResults$new(basePath=basePath)$addResult(type="spatiotemporal", shortName="ST")$results[["ST"]]$occ$getMissingDataValuesProportion(exclude.imputation.columns)
