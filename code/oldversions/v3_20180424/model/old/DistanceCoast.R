library(rgdal)
library(rgeos)
library(FNN)

MapDir<-paste0(RootDir,"/data/maps")
location<-as.matrix(m2data[,c("longitude","latitude")])

distance<-ComputeDistance2Coast(location,MapDir)

m2data$distance_coast<-distance[,"distance"]
m2data$coast_lon<-distance[,"lon"]
m2data$coast_lat<-distance[,"lat"]

# bounding box
wgs.84    <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
ukbbx <- readWKT("POLYGON((-7.5600 49.9600, 1.7800 49.9600, 1.7800 60.8400, -7.5600 60.8400, -7.5600 49.9600))",
               p4s=CRS(wgs.84))
ukgrid = "+init=epsg:27700"

# 1) Load data on rail stations
# 2) COnvert to latitude and longitude
# 3) save as SpatialPointsDataFrame
railfile<-paste0(RootDir,"/data/rail/estimates-of-station-usage-2016-17.csv")
stations<-read.csv(railfile)
s1<-SpatialPoints(stations[,c("OS.Grid.Easting","OS.Grid.Northing")],
                  proj4string = CRS("+init=epsg:27700"))
s1 <- SpatialPointsDataFrame(s1, data = stations,
                                proj4string = CRS("+init=epsg:27700"))
stations<-spTransform(s1,CRS(wgs.84))
colnames(stations@coords)[colnames(stations@coords) == "OS.Grid.Easting"] <- "longitude"
colnames(stations@coords)[colnames(stations@coords) == "OS.Grid.Northing"] <- "latitude"
rm(s1)

# Create convex hull of current region
houses<-SpatialPoints(m2data[,c("longitude","latitude")],CRS(wgs.84))
region_convexhull<-gConvexHull(houses)

# Find stations that are in region_convexhull
i1<-gIntersects(stations,region_convexhull,byid=TRUE)
stations_region<-stations[i1[1,],]

# Project latitude and longitude to cartesian coordinates for (houses,stations_region)
houses_proj <- spTransform(houses, CRS("+init=epsg:27700"))
stations_proj <- spTransform(stations_region, CRS("+init=epsg:27700"))

# For each house, find distance to and index of nearest station 
distance_station <- get.knnx(coordinates(stations_proj),coordinates(houses_proj),k=1)
m2data$distance_station<-distance_station$nn.dist/1000
m2data$station_lon<-stations_region@coords[distance_station$nn.index,1]
m2data$station_lat<-stations_region@coords[distance_station$nn.index,2]
traveltime<-predictTravelTime2(m2data[,c("longitude","latitude","station_lon","station_lat")],
                               destname = "station",region_id=2,TravelDir)
m2data$drive_station<-traveltime$drive_station
