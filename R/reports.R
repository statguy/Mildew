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

smooth <- function(xy, z, nrow=100, ncol=100, extend=0, scale) {
  library(raster)
  ext <- extent(min(xyz$x)-extend, max(xyz$x)+extend, min(xyz$y)-extend, max(xyz$y)+extend)
  r <- raster(ext, nrow, ncol)
  rz <- rasterize(xy, r, field=z, background=0)
  kernel <- focalWeight(r, scale, "Gauss")
  smooth <- focal(rz, w=kernel)
  return(smooth)      
}

plotMaps <- function(..., ids, border, ncol=2, size=18, border_size=0.5, extend=0, title="") {
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
      occ <<- OccupancyMildew$new(basePath=basePath)$loadResult(type, tag)
      col <<- ColonizationMildew$new(basePath=basePath)$loadResult(type, tag)
      ext <<- ExtinctionMildew$new(basePath=basePath)$loadResult(type, tag)
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
      addResult(type="glm", shortName="OL")
      addResult(type="spatiotemporal", tag="interceptonly", shortName="ST-I")
      addResult(type="spatialonly", shortName="S")
      addResult(type="temporalreplicate", shortName="TR")
      addResult(type="spatialreplicate", shortName="SR")
      addResult(type="spatiotemporal", shortName="ST")
    },
    
    addResult = function(type, tag="", shortName) {
      results[[shortName]] <<- ModelResults$new(basePath=basePath, type=type, tag=tag, shortName=shortName)
    },
    
    savePlot = function(p, name, tag) {
      ggsave(p, filename=file.path(basePath, paste(name, "-", tag, ".png", sep="")), width=8, height=4)      
    },
    
    plotYearEstimates = function(size=18, save=F) {
      library(plyr)
      library(ggplot2)
      library(grid)
      
      prepareResult <- function(mildew, predname, outcome) {
        library(reshape2)
        mildew.data <- mildew$data[!is.na(mildew$data$y),]
        mildew.data$Year <- as.factor(mildew.data$Year)
        levels(mildew.data$Year) <- substr(levels(mildew.data$Year),3,4)        
        x <- melt(ddply(mildew.data, .(Year),
                        function(x) cbind(Observed=sum(x$y)/nrow(x),
                                          Predicted=sum(x$mu)/nrow(x))), ## TODO
                  #q025=quantile(x$mu, .025), q975=quantile(x$mu, .975))),
                  #q025=sum(x$mu025), q975=sum(x$mu975))),
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
      
      result$Outcome <- factor(result$Outcome)
      
      years <- levels(result$Year)
      breaks <- years[seq(1, length(years), by=2)]
      years.numeric <- as.numeric(as.character(result$Year))
      breaks.numeric <- seq(min(years.numeric), max(years.numeric), by=2)
      labels <- sprintf(breaks.numeric, fmt="%02d")
      #print(breaks.numeric)
      #print(labels)
      p <- ggplot(result, aes(x=Year, y=value, group=interaction(variable, Data), colour=Data)) +
        geom_line(size=1, aes(linetype=Data)) + facet_wrap(~Outcome, scales="free_y") +
        #geom_line(aes(linetype=Summary), size=1) +
        ylab("Probability") + theme_bw(size) +
        theme(legend.position="bottom", legend.title=element_blank()) +
        theme(plot.margin=unit(c(0,0,-1,0), "lines")) +
        scale_x_discrete(breaks=breaks, labels=labels)
      
      print(p)
      if (save) savePlot(p, "years", "all")
      return(p)
    },
    
    selectResults = function(shortName) {
      return(results[[shortName]])
    },
    
    loadBorder = function(fileName=file.path(basePath, "alandmap_1_20000/alandmap_rough")) {
      library(maptools)
      return(readShapeSpatial(fileName))
    },
    
    plotObservedPredicted = function(size=12, save=F) {
      library(raster)
      
      getObservedPredicted <- function(mildew, extend, scale) {
        mildew.data <- mildew$data[!is.na(mildew$data$y),]
        xyz <- subset(mildew.data, select=c("Longitude", "Latitude", "y"))
        observed <- smooth(xyz[,1:2], xyz[,3], extend=extend, scale=scale)
        xyz <- subset(mildew.data, select=c("Longitude", "Latitude", "mu"))
        predicted <- smooth(xyz[,1:2], xyz[,3], extend=extend, scale=scale)
        x <- stack(observed, predicted)
        names(x) <- c("Observed", "Predicted")
        return(x)
      }
            
      .plotInternal <- function(mildew, title) {
        extend <- 500
        scale <- 500
        r <- getObservedPredicted(mildew, extend=extend, scale=scale)
        p <- plotMaps(r[[1]], r[[2]], border=border, size=size, border_size=0.1, extend=extend, title=title) +
          scale_fill_gradient(low="white", high="red")
        print(p)
        if (save) savePlot(p, "obspred", title)
      }
      
      x <- selectResults[["ST"]]      
      border <- loadBorder()
      .plotInternal(result$occ, "Occupancy")
      .plotInternal(result$col, "Colonization")
      .plotInternal(result$ext, "Extinction")
    }
    
  )
)
