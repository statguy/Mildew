theme_raster <- function(base_size=12) {
  require(grid)
  theme(
    panel.background=element_blank(),
    panel.border=element_rect(colour="grey", fill=NA),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    panel.margin=unit(0, "lines"),
    axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.ticks=element_blank(),
    plot.margin=unit(c(0,0,-1,-1), "lines"),
    aspect.ratio=1,
    legend.position="none"
  )
}

pointsToRaster <- function(r, xyz) {
  x.scale <- (dim(r)[1] - 1) / (xmax(r) - xmin(r))
  y.scale <- (dim(r)[2] - 1) / (ymax(r) - ymin(r))
  x.trans <- xmin(r)
  y.trans <- ymin(r)
  xy.raster <- cbind(round(cbind(x.scale * (xyz[,1] - x.trans),
                                y.scale * (xyz[,2] - y.trans)) + 1))
  m.raster <- matrix(0, nrow=dim(r)[1], ncol=dim(r)[2])
  for (i in 1:nrow(xy.raster)) {
    m.raster[xy.raster[i,1], xy.raster[i,2]] <- m.raster[xy.raster[i,1], xy.raster[i,2]] + xyz[i,3]
  }
  values(r) <- t(m.raster[,ncol(m.raster):1])
  #r <- setValues(r, m.raster)
  return(r)
}

smooth <- function(xy, z, rows=400, cols=400, extend=0, scale) {
  library(raster)
  ext <- extent(min(xy[,1]) - extend, max(xy[,1]) + extend, min(xy[,2]) - extend, max(xy[,2]) + extend)
  r <- raster(ext, rows, cols)
  r <- pointsToRaster(r, cbind(xy, z))
  #r <- rasterize(xy, r, field=z, background=0, fun=mean)
  kernel <- focalWeight(r, scale, "Gauss")
  smooth <- focal(r, w=kernel)
  return(smooth)      
}

plotRasterMaps <- function(..., ids, border, ncol=2, size=18, border_size=0.5, extend=0, title="") {
  library(raster)
  library(ggplot2)
  library(plyr)
  
  df <- ldply(list(...), function(r, border, title) {
    message("Masking and converting ", names(r), " of ", title, "...")
    r.masked <- raster::mask(r, border)
    x <- as.data.frame(rasterToPoints(r.masked))
    names(x) <- c("x","y","fill")
    x$id <- names(r)
    return(x)
  }, border=border, title=title, .parallel=TRUE)
  
  border.fed <- ldply(list(...), function(r, border.fed) {
    border.fed$id <- names(r)
    return(border.fed)
  }, border.fed=fortify(border))
  
  p <- ggplot(df, aes(x=x, y=y, fill=fill)) +
    geom_raster() +
    facet_wrap(~id, ncol=2) +
    geom_path(data=border.fed, aes(x=long, y=lat, group=group, fill=NULL), colour="black", size=border_size) +
    coord_fixed(xlim=border@bbox[1,] + c(-1,1) * extend, ylim=border@bbox[2,] + c(-1,1) * extend) +
    ggtitle(title) +
    theme(text=element_text(size=size)) +
    theme_raster()
  return(p)
}

ModelResults <- setRefClass(
  "ModelResults",
  fields = list(
    occ = "OccupancyMildew",
    col = "ColonizationMildew",
    ext = "ExtinctionMildew",
    shortName = "character",
    longName = "character"
  ),
  methods = list(
    initialize = function(basePath, type, tag="", shortName, ...) {
      callSuper(...)
      if (missing(basePath) | missing(type) | missing(shortName))
        stop("Missing argument.")
      shortName <<- shortName
      occ <<- OccupancyMildew$new(basePath=basePath)$loadResult(type, tag) #$removeUndefined()
      col <<- ColonizationMildew$new(basePath=basePath)$loadResult(type, tag)$loadUndefined()
      ext <<- ExtinctionMildew$new(basePath=basePath)$loadResult(type, tag)$loadUndefined()
    }
  )
)

MildewResults = setRefClass(
  "MildewResults",
  fields = list(
    results = "list",
    basePath = "character"
  ),
  methods = list(
    initialize = function(basePath, ...) {
      callSuper(...)
      if (missing(basePath))
        stop("Missing argument.")
      basePath <<- basePath
      return(invisible(.self))
    },

    addAllResults = function() {
      addResult(type="glm", shortName="OL")
      addResult(type="spatiotemporal", tag="interceptonly", shortName="ST-I")
      addResult(type="spatialonly", shortName="S")
      addResult(type="temporalreplicate", shortName="TR")
      addResult(type="spatialreplicate", shortName="SR")
      addResult(type="spatiotemporal", shortName="ST")
      return(invisible(.self))
    },
    
    addResult = function(type, tag="", shortName) {
      results[[shortName]] <<- ModelResults$new(basePath=basePath, type=type, tag=tag, shortName=shortName)
      return(invisible(.self))
    },
    
    summary = function() {
      library(plyr)

      .summaryInternal <- function(x, outcome, tag) {
        message("Summary for ", tag, " of ", outcome)
        x$estimateMu()$summaryResult()$summaryHyperparameters()$summaryPredictionAccuracy(0.5)$summaryPredictionAccuracy(0.1)$summaryVariance()
      }
      
      l_ply(results, function(x) {
        .summaryInternal(x$occ, "Occupancy", x$shortName)
        .summaryInternal(x$col, "Colonization", x$shortName)
        .summaryInternal(x$ext, "Extinction", x$shortName)
      })
    },
    
    savePlot = function(p, name, tag) {
      fileName <- file.path(basePath, paste(name, "-", tag, ".png", sep=""))
      message("Saving plot ", fileName, "...")
      ggsave(p, filename=fileName, width=8, height=4)
    },
    
    plotYearEstimates = function(size=12, save=F) {
      library(plyr)
      library(ggplot2)
      library(grid)
      
      prepareResult <- function(mildew, predname, outcome) {
        message("Processing ", predname, " of ", outcome, "...")
        library(reshape2)
        mildew.data <- mildew$data[!is.na(mildew$data$y),]
        mildew.data$Year <- as.factor(mildew.data$Year)
        levels(mildew.data$Year) <- substr(levels(mildew.data$Year),3,4)        
        x <- melt(ddply(mildew.data, .(Year),
                        function(x) cbind(Observed=sum(x$y)/nrow(x),
                                          Predicted=sum(x$mu)/nrow(x))),
                  id.vars="Year")
        x$Data <- as.factor(!(x$variable %in% "Observed"))
        levels(x$Data) <- c("Observed", predname)
        x$Summary <- as.factor(!(x$variable %in% c("Observed","Predicted")))
        levels(x$Summary) <- c("Mean","95% quantiles")
        x$Outcome <- outcome
        return(x)
      }
      
      result <- ldply(results, function(x) {
        rbind(prepareResult(x$occ, x$shortName, "Occupancy"),
              prepareResult(x$col, x$shortName, "Colonization"),
              prepareResult(x$ext, x$shortName, "Extinction"))
      })
      
      result$Outcome <- factor(result$Outcome, levels=c("Occupancy","Colonization","Extinction"))
      
      years <- levels(result$Year)
      breaks <- years[seq(1, length(years), by=2)]
      years.numeric <- as.numeric(as.character(result$Year))
      breaks.numeric <- seq(min(years.numeric), max(years.numeric), by=2)
      labels <- sprintf(breaks.numeric, fmt="%02d")
      #print(breaks.numeric)
      #print(labels)
      p <- ggplot(result, aes(x=Year, y=value, group=interaction(variable, Data), colour=Data)) +
        geom_line(size=1, aes(linetype=Data)) + facet_wrap(~Outcome, scales="free_y") +
        ylab("Probability") + theme_bw(size) +
        theme(legend.position="bottom", legend.title=element_blank()) +
        theme(plot.margin=unit(c(0,0,-1,0), "lines")) +
        scale_x_discrete(breaks=breaks, labels=labels)
      
      print(p)
      if (save) savePlot(p, "years", "all")
      
      return(invisible(.self))
    },
    
    selectResults = function(shortName) {
      return(results[[shortName]])
    },
    
    loadBorder = function(fileName=file.path(basePath, "alandmap_1_20000/alandmap_rough")) {
      library(maptools)
      return(readShapeSpatial(fileName))
    },
    
    plotObservedPredicted = function(size=12, extend=500, scale=500, save=F) {
      library(raster)
      
      getObservedPredicted <- function(mildew, extend, scale) {
        mildew.data <- mildew$data[!is.na(mildew$data$y),]
        xyz <- subset(mildew.data, select=c("Longitude", "Latitude", "y"))
        observed <- smooth(xyz[,1:2], as.numeric(xyz[,3]), extend=extend, scale=scale)
        xyz <- subset(mildew.data, select=c("Longitude", "Latitude", "mu"))
        predicted <- smooth(xyz[,1:2], xyz[,3], extend=extend, scale=scale)
        x <- stack(observed, predicted)
        names(x) <- c("Observed", "Predicted")
        return(x)
      }
      
      .plotInternal <- function(mildew, title) {
        r <- getObservedPredicted(mildew, extend=extend, scale=scale)
        p <- plotRasterMaps(r[[1]], r[[2]], border=border, size=size, border_size=0.1, extend=extend, title=title) +
          scale_fill_gradient(low="white", high="red")
        print(p)
        if (save) savePlot(p, "obspred", title)
      }
      
      st <- selectResults("ST")
      border <- loadBorder()
      .plotInternal(st$occ, "Occupancy")
      .plotInternal(st$col, "Colonization")
      .plotInternal(st$ext, "Extinction")
      
      return(invisible(.self))
    },
    
    plotFixedRandom = function(extend=500, size=12, save=F) {
      library(ggplot2)
      
      getFixedRandom <- function(mildew) {
        library(plyr)
        
        beta <- as.matrix(mildew$result$summary.fixed[,"mean"])[-1,,drop=F] # drop intercept
        beta.index <- names(mildew$result$summary.fixed[,"mean"])[-1]
        fixed <- as.matrix(mildew$covariates[,beta.index]) %*% beta
        hotspot <- as.vector(mildew$A %*% mildew$result$summary.random$s$mean)
        
        xyz.fixed <- data.frame(x=mildew$data$Longitude, y=mildew$data$Latitude, z=fixed, t=mildew$data$Year)
        xyz.fixed.aggregated <- ddply(xyz.fixed, .(x,y), function(x) data.frame(x=x$x[1], y=x$y[1], z=mean(x$z)))
        xyz.hotspot <- data.frame(x=mildew$data$Longitude, y=mildew$data$Latitude, z=hotspot, t=mildew$data$Year)
        xyz.hotspot.aggregated <- ddply(xyz.hotspot, .(x,y), function(x) data.frame(x=x$x[1], y=x$y[1], z=mean(x$z)))
        
        xyz.fixed.aggregated$Variable <- "Fixed effects"
        xyz.hotspot.aggregated$Variable <- "Random effects"
        xyz <- rbind(xyz.fixed.aggregated, xyz.hotspot.aggregated)
        return(xyz)
      }
      
      .plotInternal <- function(mildew, title) {
        xyz <- getFixedRandom(mildew)
        p <- ggplot(xyz, aes(x=x, y=y)) +
          facet_grid(.~Variable) +
          geom_polygon(data=border.fed, aes(x=long, y=lat, group=group), fill="white") +
          geom_path(data=border.fed, aes(x=long, y=lat, group=group), colour="black", size=.1) +
          geom_point(aes(colour=z), size=2) +
          coord_fixed(xlim=border@bbox[1,] + c(-1,1) * extend, ylim=border@bbox[2,] + c(-1,1) * extend) +
          scale_colour_gradient2(low="blue", mid="grey", high="red", guide=guide_colorbar(title=NULL, direction="horizontal")) +
          theme_raster(size) +
          theme(legend.position=c(0.5, 0.1)) +
          theme(legend.background=element_rect(color="grey")) +
          ggtitle(title)
        print(p)
        if (save) savePlot(p, "fixrnd", title)
      }

      st <- selectResults("ST")
      border <- loadBorder()
      border.fed <- fortify(border)
      .plotInternal(st$occ, "Occupancy")
      .plotInternal(st$col, "Colonization")
      .plotInternal(st$ext, "Extinction")
      
      return(invisible(.self))
    },
    
    getPosteriorRange = function(mildew, title) {
      library(INLA)
      spde.result <- inla.spde2.result(mildew$result, "s", mildew$spde)  
      range.t <- inla.tmarginal(function(x) x * mildew$coords.scale / 1000, spde.result$marginals.range.nominal$range.nominal.1)
      return(cbind(Response=title, as.data.frame(unclass(range.t))))
    },
    
    savePosteriorRange = function() {
      postrange <- rbind(getPosteriorRange(results[["ST"]]$occ, "Occupancy"),
                         getPosteriorRange(results[["ST"]]$col, "Colonization"),
                         getPosteriorRange(results[["ST"]]$ext, "Extinction"))
      postrange$log10 <- log10(postrange$x)
      write.csv(file=file.path(basePath, "postrange.csv"), postrange)
      
      return(invisible(.self))
    },
    
    plotPosteriorRange = function(logscale=T, size=24, save=F) {
      library(ggplot2)
      
      postrange <- rbind(getPosteriorRange(results[["ST"]]$occ, "Occupancy"),
                         getPosteriorRange(results[["ST"]]$col, "Colonization"),
                         getPosteriorRange(results[["ST"]]$ext, "Extinction"))
      
      p <- ggplot(postrange, aes(x, y, group=Response, colour=Response)) + geom_line(size=1) +
        scale_y_continuous("Posterior marginal density") +
        theme_bw(size) + theme(legend.position="bottom",
                               panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank(),
                               axis.line = element_line(size=1, colour="black"))
      
      if (logscale)
        p <- p + scale_x_log10("Log range (km)")
      else
        p <- p + xlab("Range (km)")
      
      print(p)
      if (save) savePlot(p, "postrange", "all")
      
      return(invisible(.self))
    }
  )
)
