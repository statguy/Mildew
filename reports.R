# The MIT License (MIT)
# 
# Copyright (c) 2014 Jussi Jousimo, jvj@iki.fi
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
# the Software, and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#   
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

library(devtools)
source_url("https://raw.github.com/statguy/RSnippets/master/Cluster/Cluster.R")
source_url("https://raw.github.com/statguy/R-Mildew/master/classes.R")

basePath <- "~/phd/mildew" # Set your path to the data files here
runParallel <- TRUE

occ <- OccupancyMildew$new(basePath=basePath, runParallel=runParallel)
col <- ColonizationMildew$new(basePath=basePath, runParallel=runParallel)
ext <- ExtinctionMildew$new(basePath=basePath, runParallel=runParallel)

summaryResult <- function(occ, col, ext, type, tag="") {
  tryCatch(occ$loadResult(type, tag)$summaryResult()$summaryHyperparameters(), error=function(e) message("Error: ", e$message))
  tryCatch(col$loadResult(type, tag)$summaryResult()$summaryHyperparameters(), error=function(e) message("Error: ", e$message))
  tryCatch(ext$loadResult(type, tag)$summaryResult()$summaryHyperparameters(), error=function(e) message("Error: ", e$message))
}

summaryResult(occ, col, ext, type="glm")
summaryResult(occ, col, ext, type="spatiotemporal", tag="interceptonly")
summaryResult(occ, col, ext, type="spatialonly")
summaryResult(occ, col, ext, type="temporalreplicate")
summaryResult(occ, col, ext, type="spatialreplicate")
summaryResult(occ, col, ext, type="spatiotemporal")
