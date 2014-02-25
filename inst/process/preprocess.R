library(CNPCluster)
library(Mildew)

if (!exists("basePath") | !exists("runParallel"))
  stop("Please set basePath and runParallel parameters.")

cnpClusterStartRemote(runParallel=runParallel, hosts=cnpClusterGetHostsUkko(maxNodes=3))

exclude.distance.columns <- c("ID","rownames","Commune","PA","Col","Ext","logfallPLM2","Distance_to_shore","S","Smildew","Smildew_pers")
exclude.imputation.columns <- c(exclude.distance.columns,"y")

# Three iterations of imputation required to fill all missing values in all cases

task1 <- function() {
  occ <- OccupancyMildew$new(basePath=basePath, runParallel=runParallel)$
    loadRawData()$
    impute(exclude.distance.columns=exclude.distance.columns, exclude.imputation.columns=exclude.imputation.columns)$
    saveData()$
    impute(exclude.distance.columns=exclude.distance.columns, exclude.imputation.columns=exclude.imputation.columns)$
    saveData()$
    impute(exclude.distance.columns=exclude.distance.columns, exclude.imputation.columns=exclude.imputation.columns)$
    saveData()
  return(invisible(occ))
}

task2 <- function() {
  col <- ColonizationMildew$new(basePath=basePath, runParallel=runParallel)$
    loadRawData()$
    saveData()
  return(invisible(col))
}

task3 <- function() {
  ext <- ExtinctionMildew$new(basePath=basePath, runParallel=runParallel)$
    loadRawData()$
    saveData()
  return(invisible(ext))
}

task1()

# Estimate occupancies first as results are needed for colonizations and extinctions

task2()
task3()

#cnpClusterEval(library(Mildew))
#cnpClusterExport(c("exclude.distance.columns", "exclude.imputation.columns", "basePath", "runParallel"))
#x <- cnpClusterApplyIndependent(task1, task2, task3)
#cnpClusterStopRemote()
