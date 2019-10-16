#' Find nearest station to houses and compute travel time
#' @param houses     (n x 2) matrix of coordinates (longitude,latitude)
#' @param region_id  index for region
#' @param dirs       list of directory names
#' @param basis      c("glp","tensor","cheb") basis for travel time model
#' @return newdata   dataframe with (station_lon,station_lat,distance_station,drive_station)
#' @export
#' @examples
#' houses    <- as.matrix(m2data[,c("longitude","latitude")])
#' region_id <- 1
#' newdata<-FindNearestStation(houses,region_id,dirs)
FindNearestStation<-function(houses,region_id,dirs,basis="glp",stationtype="rail") {
#  require(rgdal)
#  require(rgeos)
#  require(FNN)
  # names of directories and files
  if (stationtype=="rail") {
    stationfile<-paste0(dirs$raildir,"/estimates-of-station-usage-2016-17.csv")
  } else if (stationtype=="tube") {
    stationfile<-paste0(dirs$raildir,"/tube_stations.csv")
  }

  # coordinates
  wgs.84    <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  ukgrid = "+init=epsg:27700"

  # 1) Load data on rail stations
  # 2) Convert to SpatialPointsDataFrame
  # 3) convert to latitude and longitude
  stations<-read.csv(stationfile)
  s1<-SpatialPoints(stations[,c("OS.Grid.Easting","OS.Grid.Northing")],
                  proj4string = CRS("+init=epsg:27700"))
  s1<-SpatialPointsDataFrame(s1, data = stations,
                             proj4string = CRS("+init=epsg:27700"))
  stations<-spTransform(s1,CRS(wgs.84))
  colnames(stations@coords)[colnames(stations@coords) == "OS.Grid.Easting"] <- "longitude"
  colnames(stations@coords)[colnames(stations@coords) == "OS.Grid.Northing"] <- "latitude"

  houses<-SpatialPoints(houses,CRS(wgs.84))

  if (stationtype=="rail") {
    # Create convex hull of coordinates for current region
    houses_convexhull<-gConvexHull(houses)

    # Find stations that are in houses_convexhull
    i1<-gIntersects(stations,houses_convexhull,byid=TRUE)

    # too few stations found, then look in region
    if (sum(i1[1,])<5) {
      # region_id    station_region
      # 1 CaMKOx     c(2,4)
      # 2 Cornwall   c()
      # 3
      # 4
      # 5
      # 6
      # 7
      # 8
      # 9
      # 10
      # 11
      i2<-c("CaMkOx","South West","East Midlands","East",
            "London","North East","North West","South East",
            "South West","West Midlands",
            "Yorkshire And The Humber")
      if (region_id==1) {
        # CaMkOx
        i1 <- ((levels(stations@data$Region)[stations@data$Region]=="East") |
               (levels(stations@data$Region)[stations@data$Region]=="South East") |
               (levels(stations@data$Region)[stations@data$Region]=="London") )
      } else {
        # extract current region from stations
        i1<-levels(stations@data$Region)[stations@data$Region]==i2[region_id]
      }
      stations_region<-stations[i1,]
    } else {
      # use only stations within convex hull of current set of houses
      stations_region<-stations[i1[1,],]
    }
  } else if (stationtype=="tube") {
    stations_region<-stations
  }
  # Project latitude and longitude to cartesian coordinates for (houses,stations_region)
  houses_proj <- spTransform(houses, CRS("+init=epsg:27700"))
  stations_proj <- spTransform(stations_region, CRS("+init=epsg:27700"))

  # For each house, find distance to and index of nearest station
  distance_station <- get.knnx(coordinates(stations_proj),coordinates(houses_proj),k=1)
  newdata<-data.frame(coordinates(houses),
                      stations_region@coords[distance_station$nn.index,1:2])
  traveltime<-predictTravelTime2(newdata,destname = "station",region_id,dirs$traveldir,
                                 printstr="station",basis=basis)

  newdata<-cbind(newdata[,3:4],distance_station$nn.dist/1000,
                 traveltime$drive_station,traveltime$trans_station)
  if (stationtype=="rail") {
    colnames(newdata)<-c("station_lon","station_lat","distance_station","drive_station",
                         "trans_station")
  } else if (stationtype=="tube") {
    colnames(newdata)<-c("tubestation_lon","tubestation_lat","distance_tubestation","drive_tubestation",
                         "trans_tubestation")
  }
  return(newdata)
}
