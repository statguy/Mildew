library(Mildew)

if (!exists("basePath"))
  stop("Please set basePath parameter.")

occ <- OccupancyMildew$new(basePath=basePath, runParallel=runParallel)$loadData()
col <- ColonizationMildew$new(basePath=basePath, runParallel=runParallel)$loadData()
ext <- ExtinctionMildew$new(basePath=basePath, runParallel=runParallel)$loadData()
occ.mesh.params <- list(min.angle=20, max.edge=c(3400,10000), cutoff=1000, coords.scale=1e6)
col.mesh.params <- list(min.angle=20, max.edge=c(3400,10000), cutoff=1200, coords.scale=1e6)
ext.mesh.params <- list(min.angle=20, max.edge=c(2700,10000), cutoff=1000, coords.scale=1e6)
occ.connectivity.scale <- 2000
col.connectivity.scale <- 2000
ext.connectivity.scale <- 500
occ.fixed.effects <- "fallPLM2 + S + road_PA + varjoisuus.L + varjoisuus.Q + Rainfall_August"
col.fixed.effects <- "fallPLM2 + S + Smildew_pers + road_PA + varjoisuus.L + varjoisuus.Q + Rainfall_August"
ext.fixed.effects <- "fallPLM2 + S + Smildew + road_PA"

estimateOrdinaryLogisticModel <- function(mildew, connectivity.scale, fixed.effects, tag="", type="glm") {
  mildew$addLandscapeConnectivity(connectivity.scale=connectivity.scale)
  mildew$addPopulationConnectivity(connectivity.scale=connectivity.scale)
  mildew$setupModel(type=type, fixed.effects=fixed.effects)
  mildew$estimate(tag=tag, saveToFile=TRUE)
}

estimateInterceptOnlyRandomEffectModel <- function(mildew, connectivity.scale, mesh.params, tag="interceptonly", type) {
  mildew$setupModel(type=type, mesh.params=mesh.params)
  mildew$estimate(tag=tag, saveToFile=TRUE)
}

estimateRandomEffectModel <- function(mildew, connectivity.scale, fixed.effects, mesh.params, tag="", type) {
  mildew$addLandscapeConnectivity(connectivity.scale=connectivity.scale)
  mildew$addPopulationConnectivity(connectivity.scale=connectivity.scale)
  mildew$setupModel(type=type, fixed.effects=fixed.effects, mesh.params=mesh.params)
  mildew$estimate(tag=tag, saveToFile=TRUE)
}

task11 <- function() estimateOrdinaryLogisticModel(occ, connectivity.scale=occ.connectivity.scale, fixed.effects=occ.fixed.effects)
task12 <- function() estimateOrdinaryLogisticModel(col, connectivity.scale=col.connectivity.scale, fixed.effects=col.fixed.effects)
task13 <- function() estimateOrdinaryLogisticModel(ext, connectivity.scale=ext.connectivity.scale, fixed.effects=ext.fixed.effects)

task21 <- function() estimateInterceptOnlyRandomEffectModel(occ, connectivity.scale=occ.connectivity.scale, mesh.params=occ.mesh.params, type="spatiotemporal")
task22 <- function() estimateInterceptOnlyRandomEffectModel(col, connectivity.scale=col.connectivity.scale, mesh.params=col.mesh.params, type="spatiotemporal")
task23 <- function() estimateInterceptOnlyRandomEffectModel(ext, connectivity.scale=ext.connectivity.scale, mesh.params=ext.mesh.params, type="spatiotemporal")

task31 <- function() estimateRandomEffectModel(occ, connectivity.scale=occ.connectivity.scale, fixed.effects=occ.fixed.effects, mesh.params=occ.mesh.params, type="spatialonly")
task32 <- function() estimateRandomEffectModel(col, connectivity.scale=col.connectivity.scale, fixed.effects=col.fixed.effects, mesh.params=col.mesh.params, type="spatialonly")
task33 <- function() estimateRandomEffectModel(ext, connectivity.scale=ext.connectivity.scale, fixed.effects=ext.fixed.effects, mesh.params=ext.mesh.params, type="spatialonly")

task41 <- function() estimateRandomEffectModel(occ, connectivity.scale=occ.connectivity.scale, fixed.effects=occ.fixed.effects, mesh.params=occ.mesh.params, type="temporalreplicate")
task42 <- function() estimateRandomEffectModel(col, connectivity.scale=col.connectivity.scale, fixed.effects=col.fixed.effects, mesh.params=col.mesh.params, type="temporalreplicate")
task43 <- function() estimateRandomEffectModel(ext, connectivity.scale=ext.connectivity.scale, fixed.effects=ext.fixed.effects, mesh.params=ext.mesh.params, type="temporalreplicate")

task51 <- function() estimateRandomEffectModel(occ, connectivity.scale=occ.connectivity.scale, fixed.effects=occ.fixed.effects, mesh.params=occ.mesh.params, type="spatialreplicate")
task52 <- function() estimateRandomEffectModel(col, connectivity.scale=col.connectivity.scale, fixed.effects=col.fixed.effects, mesh.params=col.mesh.params, type="spatialreplicate")
task53 <- function() estimateRandomEffectModel(ext, connectivity.scale=ext.connectivity.scale, fixed.effects=ext.fixed.effects, mesh.params=ext.mesh.params, type="spatialreplicate")

task61 <- function() estimateRandomEffectModel(occ, connectivity.scale=occ.connectivity.scale, fixed.effects=occ.fixed.effects, mesh.params=occ.mesh.params, type="spatiotemporal")
task62 <- function() estimateRandomEffectModel(col, connectivity.scale=col.connectivity.scale, fixed.effects=col.fixed.effects, mesh.params=col.mesh.params, type="spatiotemporal")
task63 <- function() estimateRandomEffectModel(ext, connectivity.scale=ext.connectivity.scale, fixed.effects=ext.fixed.effects, mesh.params=ext.mesh.params, type="spatiotemporal")


args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 2) stop("Invalid arguments.")
test <- args[1]
task_id <- args[length(args)]
message("Arguments provided:")
print(args)

if (task_id == 11) task11()
else if (task_id == 12) task12()
else if (task_id == 12) task13()
else if (task_id == 21) task21()
else if (task_id == 22) task22()
else if (task_id == 23) task23()
else if (task_id == 31) task31()
else if (task_id == 32) task32()
else if (task_id == 33) task33()
else if (task_id == 41) task41()
else if (task_id == 42) task42()
else if (task_id == 43) task43()
else if (task_id == 51) task51()
else if (task_id == 52) task52()
else if (task_id == 53) task53()
else if (task_id == 61) task61()
else if (task_id == 62) task62()
else if (task_id == 63) task63()
