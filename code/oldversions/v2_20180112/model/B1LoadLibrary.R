library(crs)            # categorical regression splines
library(ggmap)          # google maps
library(stargazer)      # output table formatting
library(margins)    
library(plyr)           # dataframe manipulation
library(splines2)       # spline library
library(np)             # nonparametric 
library(akima)          # spline interpolation
library("devtools")
#devtools::install_github("klutometis/roxygen")
library(roxygen2)

library(FNN)
library(sp)             # spatial
library(geosphere)      # spatial
library(rgeos)          # spatial tools
library(rgdal)      # Geospatial Data Abstraction Library 
library(maptools)   # Tools for working with maps

# Set up user defined R package "nictools"
install(paste0(CodeDir,"/tools/nictools"))
d1<-getwd()
setwd(paste0(CodeDir,"/tools/nictools"))
document()
setwd(d1)
library(nictools)

library(lubridate)
library(class)        # for classification
library(xtable)
library(tools)      # basic R tools for checking filenames
library(parallel)
# library(dplyr)

# Define method to convert SpatialLinesDataFame to SpatialPointsDataFrame
setAs("SpatialLinesDataFrame", "SpatialPointsDataFrame", function(from) {
  spp = as(as(from, "SpatialLines"), "SpatialPointsDataFrame")
  dfl = from@data[spp$Lines.NR, , drop = FALSE]
  spp@data = cbind(dfl, spp@data)
  spp
}
)

# Define method to convert SpatialPolygonsDataFrame to SpatialLinesDataFrame
setAs("SpatialPolygonsDataFrame", "SpatialLinesDataFrame", 
      function(from) SpatialLinesDataFrame(as(from, "SpatialLines"),
                                           from@data, match.ID = FALSE))
