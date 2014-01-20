library(INLA)
setOldClass("inla.mesh")
setOldClass("inla.spde2")
setOldClass("inla.data.stack")
setOldClass("inla")

OccupancyMildew <- setRefClass(
  "OccupancyMildew",
  fields = list(
    basePath = "character",
    runParallel = "logical",
    
    data = "data.frame",
    response = "character",
    type = "character",
    tag = "character",
    coords.scale = "numeric",
    mesh = "inla.mesh",
    spde = "inla.spde2",
    index = "list",
    A = "Matrix",
    group.years = "integer",
    
    covariates = "ANY",
    model = "formula",
    data.stack = "inla.data.stack",
    result = "inla"
  ),
  methods = list(
    initialize = function(runParallel=FALSE, response="occupancy", ...) {
      callSuper(response=response, runParallel=runParallel, ...)
      return(.self)
    },

    mergeRainfall = function(mildew, rainfallFile="W_uni.csv") {
      message("Loading rainfall data...")
      rainfall <- read.csv(file.path(basePath, rainfallFile))
      return(merge(mildew, rainfall, sort=FALSE, all.x=TRUE, all.y=FALSE, by=c("ID", "Year")))
    },  
    
    loadRawData = function(mildewFile="SO_with_covariates_univariate_2001_2012.csv") {
      message("Loading ", response, " data...")
      
      mildew <- read.csv(file.path(basePath, mildewFile))
      mildew <- transform(mildew, y=as.logical(PA), road_PA=as.logical(road_PA), Open_bin=as.logical(Open_bin), varjoisuus=factor(varjoisuus),
                          fallPLM2=fallPLM2, Distance_to_shore=Distance_to_shore, fallPLdry=fallPLdry,
                          logfallPLM2=log(fallPLM2), logDistance_to_shore=log(Distance_to_shore), S=S)
      mildew <- mergeRainfall(mildew)
      mildew$fallPLdry[mildew$fallPLdry > 100] <- NA
      mildew$varjoisuus[mildew$varjoisuus == 0] <- NA
      
      data <<- mildew
      return(.self)
    },

    # Imputation by k nearest neighbors regressiong using Gower's distance to allow categorical variables
    impute = function(k=10, aggregation.function=median, distance.metric="gower", exclude.distance.columns=NULL, exclude.imputation.columns=NULL) {
      require(cluster)
      aggregation.function <- match.fun(aggregation.function)
      
      #distance.values <- matrix()
      #distance.indices <- matrix()
      #x <- list()
      #for (i in !complete.cases)
      #  x[[i]] <- distance.lookup[i,order(distance.lookup[i,])][k_seq]
      
      #sub <- occ$data[3000:3100,] #c("ID","Year","Latitude","Longitude","road_PA","Open_bin","Distance_to_shore","Rainfall_July","Rainfall_August",  "y","fallPLM2","fallPLdry","varjoisuus")]
      
      data.distance <- data[, !(colnames(data) %in% exclude.distance.columns), drop=FALSE]
      data.imputed <- data[, !(colnames(data) %in% exclude.imputation.columns), drop=FALSE]
      data.numeric <- data.matrix(data.imputed)
      
      message("Calculating distances for imputation...")
      distance.lookup <- as.matrix(daisy(data.distance, metric=distance.metric))
      #column.classes <- character(ncol(data.imputed))
      #for (column in seq_along(data.imputed)) column.classes[column] <- class(data.imputed[, column])
      
      message("Imputing...")
      k.seq <- 2:(k+1)
      missing.rows <- which(!complete.cases(data.numeric))
      for (missing.row in missing.rows) {
        nearest.neighbor.rows <- names(distance.lookup[missing.row, order(distance.lookup[missing.row,])[k.seq]])
        missing.columns <- which(is.na(data.numeric[missing.row,]))        
        for (missing.column.index in 1:length(missing.columns)) {
          missing.column <- missing.columns[missing.column.index]
          neighbor.values <- data.numeric[nearest.neighbor.rows, missing.column]
          imputed.value <- aggregation.function(neighbor.values, na.rm=TRUE)
          if (is.na(imputed.value)) {
            warning("Could not impute missing value on row = ", missing.row, ", column = ", missing.column, ": all neighboring values are NA. Consider increasing k or iterating the imputation several times.")
          }
          else {
            #message("row = ", missing.row, ", column = ", missing.column, ", column.class = ", class(data.imputed[, missing.column]), ", imputed.value = ", imputed.value, " from values = ", paste(neighbor.values, collapse=" "))
            data.imputed[missing.row, missing.column] <- switch(class(data.imputed[, missing.column]),
              factor = round(imputed.value),
              logical = as.logical(imputed.value),
              numeric = imputed.value,
              integer = as.integer(imputed.value))
          }
        }
      }
      
      data <<- cbind(data[, colnames(data) %in% exclude.imputation.columns, drop=F], data.imputed)[, colnames(data)]
      return(.self)
    },
    
    getDataFileName = function() {
      return(file.path(basePath, paste("MildewData-", response, ".RData", sep="")))
    },
    
    saveData = function() {
      save(data, file=getDataFileName())
    },
    
    loadData = function() {
      load(getDataFileName(), envir=as.environment(.self))
      return(.self)
    },
    
    connectivity = function(z1, z2, area, alpha, occurrence=1) {
      return(sum(exp(-alpha * Mod(z2 - z1)) * sqrt(area) * occurrence, na.rm=T))
    },
    
    addLandscapeConnectivity = function(connectivity.scale) {
      require(plyr)

      message("Computing landscape connectivity...")
      
      # Find mean patch coverage for each patch across all years
      x <- ddply(data, .(ID), function(x) {
          data.frame(Z=complex(real=x$Latitude, imaginary=x$Longitude)[1], A=mean(x$fallPLM2))
        }, .parallel=runParallel)
      
      # Landscape connectivity
      x$S <- NA
      for (i in 1:nrow(x)) {
        x$S[i] <- connectivity(x$Z[i], x$Z[-i], x$A[-i], 1 / connectivity.scale)
      }
      
      data$S <<- NULL
      y <- merge(data, x[,c("ID","S")], by="ID", sort=F)
      data <<- y
      
      return(.self)
    },
    
    getPersistence = function() {
      # Check whether mildew survived over the winter
            
      occupancy <- if (class(.self) == "OccupancyMildew") .self
      else {
        x <- OccupancyMildew(basePath=basePath, runParallel=runParallel)
        x$loadData()
        x
      }
      
      message("Computing persistence...")
      
      persistence <- c()
      years <- sort(unique(occupancy$data$Year))[-1]
      persistence <- ldply(years, function(year, occ.data) {        
        message("Processing year ", year, "...")
        
        x1 <- subset(occ.data, Year==year-1)
        x2 <- subset(occ.data, Year==year)
        x2$persistent <- NA
        for (i in 1:nrow(x2)) {
          j <- which(x1$ID == x2$ID[i])
          if (length(j)==1)
            x2$persistent[i] <- x1$PA[j]==1 & x2$PA[i]==1
        }
        return(x2)
      }, occ.data=occupancy$data, .parallel=runParallel)
      
      occupancy$data$persistent <- NULL
      occupancy$data <- merge(occupancy$data, persistence[,c("ID","Year","persistent")], by=c("ID","Year"), all.x=T, sort=F)
      return(occupancy$data)
    },

    addPopulationConnectivity = function(connectivity.scale) {
      require(plyr)
      
      persistence <- getPersistence()
      
      message("Computing population connectivity...")
      
      # Population connectivity and persistent population connectivity
      x <- ddply(persistence, .(Year), function(x, scale) {
        message("Processing year ", x$Year[1], "...")
        
        Z <- complex(real=x$Longitude, imaginary=x$Latitude)
        x$Smildew <- NA
        x$Smildew_pers <- NA
        for (i in 1:nrow(x)) {
          x$Smildew[i] <- connectivity(Z[i], Z[-i], x$fallPLM2[-i], 1 / scale, x$PA[-i])
          x$Smildew_pers[i] <- connectivity(Z[i], Z[-i], x$fallPLM2[-i], 1 / scale, x$persistent[-i])
        }
        return(x)
      }, scale=connectivity.scale, .parallel=runParallel)
      
      data$Smildew <<- NULL
      data$Smildew_pers <<- NULL
      y <- merge(data, x[,c("ID","Year","Smildew","Smildew_pers")], by=c("ID","Year"))      
      data <<- y
      
      return(.self)
    },
    
    reid = function(id) {
      newid <- integer(length(id))
      hash <- list()
      count <- 0
      for (i in 1:length(id)) {
        if (is.null(hash[[as.character(id[i])]])) {
          count <- count + 1
          hash[[as.character(id[i])]] <- count
          newid[i] <- count        
        }
        else {
          newid[i] <- hash[[as.character(id[i])]]
        }
      }
      return(newid)
    },
    
    setupModel = function(type, scale.covariates=TRUE, fixed.effects, mesh.params, plot=FALSE) {
      type <<- type
      #data <<- data[complete.cases(data),]
      
      random.effects <- switch(type,
        glm=NULL,
        temporalreplicate2="f(s, model='iid', group=s.group, control.group=list(model='ar1'))",
        spatialreplicate2="f(s, model=spde, group=s.group, control.group=list(model='ar1', hyper=list(theta=list(initial=0, fixed=T))))",
        spatiotemporal="f(s, model=spde, group=s.group, control.group=list(model='ar1'))",
        spatialonly="f(s, model=spde)",
        temporalonly="f(data$ID, model='iid', group=s.group, control.group=list(model='ar1'))",
        spatialreplicate="f(s, model=spde, replicate=s.repl)",
        temporalreplicate="f(data$ID, model='ar1', replicate=group.years)")
      
      if (missing(fixed.effects)) {
        model <<- as.formula(paste(c("y ~ -1 + intercept", random.effects), collapse=" + "))
        covariates <<- NULL
      }
      else {
        model <<- as.formula(paste(c("y ~ -1 + intercept", paste(fixed.effects, collapse=" + "), random.effects), collapse=" + "))
        #data$intercept <- 1
        #covariates <<- as.data.frame(model.matrix(model, data=data[,names(data) != "y"], na.action=na.fail))
        covariates <<- as.data.frame(model.matrix(~-1+., data=data[,names(data) != "y"], na.action=na.fail))

        if (scale.covariates) {
          require(arm)
          
          message("Scaling covariates...")
          
          covariates$fallPLM2 <<- rescale(covariates$fallPLM2)
          covariates$road_PA <<- rescale(covariates$road_PA)
          covariates$Distance_to_shore <<- rescale(covariates$Distance_to_shore)
          covariates$Open_bin <<- rescale(covariates$Open_bin)    
          if (any(names(covariates) == "S")) covariates$S <<- rescale(covariates$S)
          if (any(names(covariates) == "Smildew")) covariates$Smildew <<- rescale(covariates$Smildew)
          if (any(names(covariates) == "Smildew_pers")) covariates$Smildew_pers <<- rescale(covariates$Smildew_pers)
          covariates$fallPLdy <<- rescale(covariates$fallPLdry)
          #covariates$varjoisuus <<- rescale(covariates$varjoisuus)
          covariates$Rainfall_August <<- rescale(covariates$Rainfall_August)
          covariates$Rainfall_July <<- rescale(covariates$Rainfall_July)
          covariates$logDistance_to_shore <<- rescale(covariates$logDistance_to_shore)
        }
      }
      
      years <- data$Year
      n.years <- length(unique(years))
      group.years <<- as.integer(years - min(years) + 1)
      
      if (type == "glm" | type == "temporalreplicate") return(.self)

      message("Constructing mesh...")
      
      coords.scale <<- mesh.params$coords.scale
      locations <- cbind(data$Longitude, data$Latitude) / coords.scale
      mesh <<- inla.mesh.create.helper(points.domain=locations, min.angle=mesh.params$min.angle, max.edge=mesh.params$max.edge / coords.scale, cutoff=mesh.params$cutoff / coords.scale)
      spde <<- inla.spde2.matern(mesh) 
      
      if (plot) {
        plot(mesh)
        points(locations, col="red", pch=16, cex=.1)
      }
      
      message("Number of mesh nodes = ", mesh$n) 
      
      if (type == "spatiotemporal") {
        index <<- inla.spde.make.index("s", n.spde=mesh$n, n.group=n.years)
        A <<- inla.spde.make.A(mesh, loc=locations, group=group.years, n.group=n.years)
      }
      else if (type == "spatialonly") {
        index <<- inla.spde.make.index("s", n.spde=mesh$n)
        A <<- inla.spde.make.A(mesh, loc=locations)
      }
      else if (type == "spatialreplicate") {
        index <<- inla.spde.make.index("s", n.spde=mesh$n, n.repl=n.years)
        data$reID <<- reid(data$ID)
        A <<- inla.spde.make.A(mesh, loc=locations, index=data$reID, repl=group.years)    
      }
      else if (type == "temporalonly") {
        index <<- inla.spde.make.index("s", n.spde=mesh$n, n.group=n.years)
        A <<- inla.spde.make.A(mesh, loc=locations, group=group.years, n.group=n.years)
      }
      else stop("Unknown model '", type, "'.")
      
      return(.self)
    },
    
    invlogit = function(x) exp(x)/(1+exp(x)),

    estimate = function(tag, saveToFile=F) {
      tag <<- tag
      
      message("Estimating model ", model[2], " ", model[1], " ", model[3], "...")
      
      if (type == "glm") {
        result <<- inla(model, family="binomial",
                       data=cbind(covariates, intercept=1, y=as.numeric(data$y)),
                       verbose=TRUE,
                       control.predictor=list(compute=TRUE),
                       control.compute=list(cpo=FALSE, dic=TRUE))
        if (is.null(result$ok) || result$ok == FALSE) {
          stop("INLA failed to run.")
        }
        
        data$mu <<- invlogit(result$summary.linear.predictor$mean)
      }
      else if (type == "temporalreplicate") {
        result <<- inla(model, family="binomial",
                       data=cbind(covariates, intercept=1, y=as.numeric(data$y)),
                       verbose=TRUE,
                       control.predictor=list(compute=TRUE),
                       control.compute=list(cpo=FALSE, dic=TRUE))
        if (is.null(result$ok) || result$ok == FALSE) {
          stop("INLA failed to run.")
        }
        
        data.full <- expand.grid(ID=unique(data$ID), Year=unique(data$Year))  
        data.full$random <- result$summary.random$"data$ID"$mean
        data <<- merge(data, data.full)
        
        data$mu <<- invlogit(result$summary.linear.predictor$mean)
      }
      else {
        data.stack <<- if (is.null(covariates)) {
          inla.stack(data=list(y=as.numeric(data$y)),
            A=list(A),
            effects=list(c(index, list(intercept=1))),
            tag="pred")
        }
        else {
          inla.stack(data=list(y=as.numeric(data$y)),
            A=list(A, 1),
            effects=list(c(index, list(intercept=1)), covariates),
            tag="pred")
        }
        
        result <<- inla(model, family="binomial", data=inla.stack.data(data.stack),
                       verbose=TRUE,
                       control.predictor=list(A=inla.stack.A(data.stack), compute=TRUE),
                       control.compute=list(cpo=FALSE, dic=TRUE))
        if (is.null(result$ok) || result$ok == FALSE) {
          stop("INLA failed to run.")
        }
        
        if (type != "temporalonly") {
          data$random <<- as.vector(A %*% result$summary.random$s$mean)
        }
        
        index.pred <- inla.stack.index(data.stack, "pred")$data
        data$mu <<- invlogit(result$summary.linear.predictor$mean[index.pred])
      }

      # TODO
      #data$fixed <<- 
      data$residual <<- data$y - data$mu

      message("Finished estimating ", response, "-", type, "-", tag)
      
      if (saveToFile) {
        saveResult()
      }
    },
    
    getResultFileName = function(type, tag) {
      return(file.path(basePath, paste("MildewResult-", response, "-", type, "-", tag, ".RData", sep="")))
    },
    
    saveResult = function() {
      fileName <- getResultFileName(type, tag)
      message("Saving result to ", fileName, "...")
      save(result, data, data.stack, covariates, model, mesh, spde, index, coords.scale, A, file=fileName)
    },
    
    loadResult = function(type, tag) {
      type <<- type
      tag <<- tag
      fileName <- getResultFileName(type, tag)
      message("Loading result from ", fileName, "...")      
      load(fileName, envir=as.environment(.self))
      invisible(.self)
    },
    
    summaryResult = function() {
      print(summary(result))
      invisible(.self)
    },
    
    summaryHyperparameters = function() {
      if (!any(names(result) == "summary.hyperpar")) {
        message("Model has no hyperparameters.")
      }
      else {
        spde.result <- inla.spde2.result(result, "s", spde)
        
        range.t <- inla.tmarginal(function(x) x * coords.scale, spde.result$marginals.range.nominal$range.nominal.1)
        range.e <- inla.emarginal(function(x) x, range.t)
        range.e2 <- inla.emarginal(function(x) x^2, range.t)
        range.sd <- sqrt(range.e2-range.e^2)
        range.q <- inla.qmarginal(c(0.025, 0.5, 0.975), range.t)
        
        var.t <- inla.tmarginal(function(x) x, spde.result$marginals.variance.nominal$variance.nominal.1)
        var.e <- inla.emarginal(function(x) x, var.t)
        var.e2 <- inla.emarginal(function(x) x^2, var.t)
        var.sd <- sqrt(var.e2-var.e^2)
        var.q <- inla.qmarginal(c(0.025, 0.5, 0.975), var.t)
        
        kappa.t <- inla.tmarginal(function(x) x / coords.scale, spde.result$marginals.kappa$kappa.1)
        kappa.e <- inla.emarginal(function(x) x, kappa.t)
        kappa.e2 <- inla.emarginal(function(x) x^2, kappa.t)
        kappa.sd <- sqrt(kappa.e2-kappa.e^2)
        kappa.q <- inla.qmarginal(c(0.025, 0.5, 0.975), kappa.t)
        
        tau.t <- inla.tmarginal(function(x) x * coords.scale, spde.result$marginals.tau$tau.1)
        tau.e <- inla.emarginal(function(x) x, tau.t)
        tau.e2 <- inla.emarginal(function(x) x^2, tau.t)
        tau.sd <- sqrt(tau.e2-tau.e^2)
        tau.q <- inla.qmarginal(c(0.025, 0.5, 0.975), tau.t)
        
        y <- rbind(kappa=c(kappa.e, kappa.sd, kappa.q),
                   tau=c(tau.e, tau.sd, tau.q),
                   range=c(range.e, range.sd, range.q),
                   variance=c(var.e, var.sd, var.q))
        if (any(rownames(result$summary.hyperpar)=="GroupRho for s"))
          y <- rbind(y, rho=result$summary.hyperpar["GroupRho for s",])
        colnames(y) <- c("mean","sd","0.025quant","0.5quant","0.975quant")
        print(y)
        invisible(y)
      }
      invisible(.self)
    },
    
    loadBorder = function(fileName=file.path(basePath, "alandmap_1_20000/alandmap_rough")) {
      require(sp)
      require(maptools)
      return(readShapeSpatial(fileName))
    },
    
    plotMesh = function(borderFileName) {
      if (is.null(mesh)) stop("Mesh has not been set up.")
      
      border <- if (missing(borderFileName)) loadBorder()
      else loadBorder(fileName=borderFileName)
      
      t.sub <- 1:nrow(mesh$graph$tv)
      xlim <- range(mesh$loc[,1]) * coords.scale
      ylim <- range(mesh$loc[,2]) * coords.scale
      idx <- t(cbind(mesh$graph$tv[t.sub, c(1:3, 1), drop = FALSE], NA))
      x <- mesh$loc[idx, 1] * coords.scale
      y <- mesh$loc[idx, 2] * coords.scale
      
      plot.new()
      plot.window(xlim = xlim, ylim = ylim, "", asp=1)
      lines(x, y, type = "l", col = "gray", lwd=3)
      plot(border, add=T, border="black", lwd=6)
      points(unique(cbind(data$Longitude, data$Latitude)), pch=20, col="red")
      
      invisible(.self)
    },
    
    saveDataCSV = function(fileName) {
      write.csv(data, file=fileName)
    }
  )
)

ColonizationMildew <- setRefClass(
  "ColonizationMildew",
  contains = "OccupancyMildew",
  fields = list(
  ),
  methods = list(
    initialize = function(...) {
      callSuper(response="colonization", ...)
      return(.self)
    },
    
    loadData = function(mildewFile="SO_col_univariate_2001_2012.csv") {
      message("Loading ", response, " data...")
      
      mildew <- read.csv(file.path(basePath, mildewFile))
      mildew <- transform(mildew, y=as.logical(Col), road_PA=as.logical(road_PA), Open_bin=as.logical(Open_bin), varjoisuus=factor(varjoisuus),
                          fallPLM2=fallPLM2, Distance_to_shore=Distance_to_shore, fallPLdry=fallPLdry,
                          logfallPLM2=log(fallPLM2), logDistance_to_shore=log(Distance_to_shore), S=S)
      mildew <- mergeRainfall(mildew)
      mildew$fallPLdry[mildew$fallPLdry > 100] <- NA
      mildew$varjoisuus[mildew$varjoisuus == 0] <- NA
      
      data <<- mildew
      return(.self)
    }
  )
)

ExtinctionMildew <- setRefClass(
  "ExtinctionMildew",
  contains = "OccupancyMildew",
  fields = list(
  ),
  methods = list(
    initialize = function(...) {
      callSuper(response="extinction", ...)
      return(.self)
    },
    
    loadData = function(mildewFile="SO_ext_univariate_2001_2012.csv") {
      message("Loading ", response, " data...")
      
      mildew <- read.csv(file.path(basePath, mildewFile))
      mildew <- transform(mildew, y=as.logical(Ext), road_PA=as.logical(road_PA), Open_bin=as.logical(Open_bin), varjoisuus=factor(varjoisuus),
                          fallPLM2=fallPLM2, Distance_to_shore=Distance_to_shore, fallPLdry=fallPLdry,
                          logfallPLM2=log(fallPLM2), logDistance_to_shore=log(Distance_to_shore), S=S)
      mildew <- mergeRainfall(mildew)
      mildew$fallPLdry[mildew$fallPLdry > 100] <- NA
      mildew$varjoisuus[mildew$varjoisuus == 0] <- NA
            
      data <<- mildew
      return(.self)
    }
  )
)
