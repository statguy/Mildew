library(Mildew)

if (!exists("basePath"))
  stop("Please set basePath parameter.")

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



args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 2) stop("Invalid arguments.")
test <- args[1]
task_id <- args[length(args)]
message("Arguments provided:")
print(args)

# Estimate occupancies first as results are needed for colonizations and extinctions

if (task_id == 1) task1()
else if (task_id == 2) task2()
else if (task_id == 3) task3()
