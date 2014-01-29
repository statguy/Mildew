library(CNPCluster)

basePath <- "~/phd/mildew/data" # Set your path to the data files here
runParallel <- TRUE

cnpClusterStartLocal(runParallel=runParallel)

occ <- OccupancyMildew$new(basePath=basePath, runParallel=runParallel)$loadData()
col <- ColonizationMildew$new(basePath=basePath, runParallel=runParallel)$loadData()
ext <- ExtinctionMildew$new(basePath=basePath, runParallel=runParallel)$loadData()
occ.mesh.params <- list(min.angle=20, max.edge=c(3400,10000), cutoff=1000, coords.scale=1e6)
col.mesh.params <- list(min.angle=20, max.edge=c(3300,10000), cutoff=1000, coords.scale=1e6)
ext.mesh.params <- list(min.angle=20, max.edge=c(2800,10000), cutoff=1000, coords.scale=1e6)
occ.connectivity.scale <- 2000
col.connectivity.scale <- 2000
ext.connectivity.scale <- 1000
occ.fixed.effects <- "fallPLM2 + S + road_PA + varjoisuus2 + varjoisuus3 + Rainfall_August"
col.fixed.effects <- "fallPLM2 + S + Smildew_pers + road_PA + varjoisuus2 + varjoisuus3 + Rainfall_August"
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

estimateOrdinaryLogisticModel(occ, connectivity.scale=occ.connectivity.scale, fixed.effects=occ.fixed.effects)
estimateOrdinaryLogisticModel(col, connectivity.scale=col.connectivity.scale, fixed.effects=col.fixed.effects)
estimateOrdinaryLogisticModel(ext, connectivity.scale=ext.connectivity.scale, fixed.effects=ext.fixed.effects)

estimateInterceptOnlyRandomEffectModel(occ, connectivity.scale=occ.connectivity.scale, mesh.params=occ.mesh.params, type="spatiotemporal")
estimateInterceptOnlyRandomEffectModel(col, connectivity.scale=col.connectivity.scale, mesh.params=col.mesh.params, type="spatiotemporal")
estimateInterceptOnlyRandomEffectModel(ext, connectivity.scale=ext.connectivity.scale, mesh.params=ext.mesh.params, type="spatiotemporal")

estimateRandomEffectModel(occ, connectivity.scale=occ.connectivity.scale, fixed.effects=occ.fixed.effects, mesh.params=occ.mesh.params, type="spatialonly")
estimateRandomEffectModel(col, connectivity.scale=col.connectivity.scale, fixed.effects=col.fixed.effects, mesh.params=col.mesh.params, type="spatialonly")
estimateRandomEffectModel(ext, connectivity.scale=ext.connectivity.scale, fixed.effects=ext.fixed.effects, mesh.params=ext.mesh.params, type="spatialonly")

estimateRandomEffectModel(occ, connectivity.scale=occ.connectivity.scale, fixed.effects=occ.fixed.effects, mesh.params=occ.mesh.params, type="temporalreplicate")
estimateRandomEffectModel(col, connectivity.scale=col.connectivity.scale, fixed.effects=col.fixed.effects, mesh.params=col.mesh.params, type="temporalreplicate")
estimateRandomEffectModel(ext, connectivity.scale=ext.connectivity.scale, fixed.effects=ext.fixed.effects, mesh.params=ext.mesh.params, type="temporalreplicate")

estimateRandomEffectModel(occ, connectivity.scale=occ.connectivity.scale, fixed.effects=occ.fixed.effects, mesh.params=occ.mesh.params, type="spatialreplicate")
estimateRandomEffectModel(col, connectivity.scale=col.connectivity.scale, fixed.effects=col.fixed.effects, mesh.params=col.mesh.params, type="spatialreplicate")
estimateRandomEffectModel(ext, connectivity.scale=ext.connectivity.scale, fixed.effects=ext.fixed.effects, mesh.params=ext.mesh.params, type="spatialreplicate")

estimateRandomEffectModel(occ, connectivity.scale=occ.connectivity.scale, fixed.effects=occ.fixed.effects, mesh.params=occ.mesh.params, type="spatiotemporal")
estimateRandomEffectModel(col, connectivity.scale=col.connectivity.scale, fixed.effects=col.fixed.effects, mesh.params=col.mesh.params, type="spatiotemporal")
estimateRandomEffectModel(ext, connectivity.scale=ext.connectivity.scale, fixed.effects=ext.fixed.effects, mesh.params=ext.mesh.params, type="spatiotemporal")

cnpClusterStopLocal()
