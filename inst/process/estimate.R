library(CNPCluster)
library(Mildew)

basePath <- "~/phd/mildew/data" # Set your path to the data files here
runParallel <- TRUE

occ <- OccupancyMildew$new(basePath=basePath, runParallel=runParallel)$loadData()
col <- ColonizationMildew$new(basePath=basePath, runParallel=runParallel)$loadData()
ext <- ExtinctionMildew$new(basePath=basePath, runParallel=runParallel)$loadData()
occ.mesh.params <- list(min.angle=20, max.edge=c(3400,10000), cutoff=1000, coords.scale=1e6)
col.mesh.params <- list(min.angle=20, max.edge=c(3300,10000), cutoff=1000, coords.scale=1e6)
ext.mesh.params <- list(min.angle=20, max.edge=c(2800,10000), cutoff=1000, coords.scale=1e6)
occ.connectivity.scale <- 2000
col.connectivity.scale <- 2000
ext.connectivity.scale <- 500
#occ.fixed.effects <- "fallPLM2 + S + road_PA + varjoisuus2 + varjoisuus3 + Rainfall_August"
occ.fixed.effects <- "fallPLM2 + S + road_PA + varjoisuus.L + varjoisuus.Q + Rainfall_August"
#col.fixed.effects <- "fallPLM2 + S + Smildew_pers + road_PA + varjoisuus2 + varjoisuus3 + Rainfall_August"
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

cnpClusterStartRemote(runParallel=runParallel, hosts=cnpClusterGetHostsUkko(maxNodes=6*3), outFile=file.path(basePath, "estimate.log"))
cnpClusterEvalRemote({ library(Mildew); library(INLA) })
cnpClusterExportRemote(c("occ", "col", "ext",
                   "occ.mesh.params", "col.mesh.params", "ext.mesh.params",
                   "occ.connectivity.scale", "col.connectivity.scale", "ext.connectivity.scale",
                   "occ.fixed.effects", "col.fixed.effects", "ext.fixed.effects",
                   "estimateOrdinaryLogisticModel", "estimateInterceptOnlyRandomEffectModel", "estimateRandomEffectModel"))
x <- cnpClusterApplyIndependent(task11, task12, task13,
                                task21, task22, task23,
                                task31, task32, task33,
                                task41, task42, task43,
                                task51, task52, task53,
                                task61, task62, task63)
cnpClusterStopRemote()
