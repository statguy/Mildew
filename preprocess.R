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

exclude.distance.columns <- c("ID","rownames","Commune","PA","Col","Ext","logfallPLM2","Distance_to_shore","S","Smildew","Smildew_pers")
exclude.imputation.columns <- c(exclude.distance.columns,"y")

# Three iterations of imputation required to fill all missing values
occ <- OccupancyMildew$new(basePath=basePath, runParallel=runParallel)$
  loadRawData()$
  impute(exclude.distance.columns=exclude.distance.columns, exclude.imputation.columns=exclude.imputation.columns)$
  saveData()$
  impute(exclude.distance.columns=exclude.distance.columns, exclude.imputation.columns=exclude.imputation.columns)$
  saveData()$
  impute(exclude.distance.columns=exclude.distance.columns, exclude.imputation.columns=exclude.imputation.columns)$
  saveData()
col <- ColonizationMildew$new(basePath=basePath, runParallel=runParallel)$
  loadRawData()$
  impute(exclude.distance.columns=exclude.distance.columns, exclude.imputation.columns=exclude.imputation.columns)$
  saveData()$
  impute(exclude.distance.columns=exclude.distance.columns, exclude.imputation.columns=exclude.imputation.columns)$
  saveData()$
  impute(exclude.distance.columns=exclude.distance.columns, exclude.imputation.columns=exclude.imputation.columns)$
  saveData()
ext <- ExtinctionMildew$new(basePath=basePath, runParallel=runParallel)$
  loadRawData()$
  impute(exclude.distance.columns=exclude.distance.columns, exclude.imputation.columns=exclude.imputation.columns)$
  saveData()$
  impute(exclude.distance.columns=exclude.distance.columns, exclude.imputation.columns=exclude.imputation.columns)$
  saveData()$
  impute(exclude.distance.columns=exclude.distance.columns, exclude.imputation.columns=exclude.imputation.columns)$
  saveData()
  

if (runParallel) {
  cluster$finalize()
}
