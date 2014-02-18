library(Mildew)

if (!exists("basePath") | !exists("runParallel"))
  stop("Please set basePath and runParallel parameters.")

mildewResults <- MildewResults$new(basePath=basePath)$addAllResults()
mildewResults$summary()
mildewResults$plotYearEstimates(save=T)
mildewResults$plotObservedPredicted(save=T)
mildewResults$plotFixedRandom(save=T)
mildewResults$plotPosteriorRange(save=T)

mildewResults <- MildewResults$new(basePath=basePath)$addResult(type="spatiotemporal", shortName="ST")$selectResults("ST")
mildewResults$occ$saveDataCSV("occupancies.csv")
mildewResults$col$saveDataCSV("colonizations.csv")
mildewResults$ext$saveDataCSV("extinctions.csv")

exclude.imputation.columns <- c("ID","rownames","Commune","PA","Col","Ext","logfallPLM2","Distance_to_shore","S","Smildew","Smildew_pers","y")
<<<<<<< HEAD
=======
mildewResults <- MildewResults$new(basePath=basePath)$addResult(type="spatiotemporal", shortName="ST")$selectResults("ST")
mildewResults$occ$saveDataCSV("occupancies.csv")
mildewResults$col$saveDataCSV("colonizations.csv")
mildewResults$ext$saveDataCSV("extinctions.csv")
>>>>>>> 69967c10558c0b75324cd25a57a1f98e3cb1f0a6
mildewResults$occ$getMissingDataValuesProportion(exclude.imputation.columns)
