houses<-SpatialPoints(m2data[,c("longitude","latitude")],CRS(wgs.84))


ComputeCoast<-function(houses,mapdir) {
  require(rgdal,geosphere)
  
  # Load shape file with coast
  coastfile<-"englandcoast"
  coast<-readOGR(MapDir,layer=coastfile)
  
  # Project latitude and longitude to cartesian coordinates for (houses,stations_region)
  houses_proj <- spTransform(houses, CRS("+init=epsg:27700"))
  coast_proj <- spTransform(coast, CRS("+init=epsg:27700"))
  
  c1<-coordinates(coast_proj)
  
  # coast of england
  c2 <- matrix(unlist(c1[[1]]), ncol = 2, byrow = TRUE)
  # For each house, find distance to and index of nearest station 
  distance_coast1<-get.knnx(c2,coordinates(houses_proj),k=1)
  
  # coast of isle of wight
  c2 <- matrix(unlist(c1[[2]]), ncol = 2, byrow = TRUE)
  distance_coast2<-get.knnx(c2,coordinates(houses_proj),k=1)
  
  distance_station <- get.knnx(coordinates(stations_proj),coordinates(houses_proj),k=1)
  m2data$distance_station<-distance_station$nn.dist/1000
  m2data$station_lon<-stations_region@coords[distance_station$nn.index,1]
  m2data$station_lat<-stations_region@coords[distance_station$nn.index,2]
  
  
  
  distance<-dist2Line(location,coast)
  return(distance)
}
