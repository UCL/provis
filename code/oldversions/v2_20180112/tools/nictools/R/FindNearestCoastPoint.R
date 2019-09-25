#' Find nearest point on coast to houses and compute travel time
#' @param houses     (n x 2) matrix of coordinates (longitude,latitude)
#' @param region_id  index for region
#' @param dirs       list of directories: (dirs$mapdir,dirs$traveldir)
#' @return newdata   dataframe with (coast_lon,coast_lat,distance_coast,drive_coast)
#' @export
#' @examples
#' houses    <- as.matrix(m2data[,c("longitude","latitude")])
#' region_id <- 1
#' dirs$mapdir <- paste0(RootDir,"/data/maps")
#' dirs$traveldir <- paste0(RootDir,"/code/output/TravelTime")
#' newdata<-FindNearestCoastPoint(houses,region_id,dirs)
FindNearestCoastPoint<-function(houses,region_id,dirs) {

  # define coordinate systems
  wgs.84    <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  ukgrid = "+init=epsg:27700"

  # Load coast from shape file
  coast<-readOGR(dirs$mapdir,layer="englandcoast")

  # Create convex hull of coordinates for current region
  houses<-SpatialPoints(houses,CRS(wgs.84))
  houses_convexhull<-gConvexHull(houses)

  # Find coastline that intersects houses_convexhull
  i1<-gIntersects(coast,houses_convexhull,byid=TRUE)

  # if too few points found, then use all, otherwise subset
  if (sum(i1[1,])>5) {
    # use only coastal points within convex hull of current set of houses
    coast<-coast[i1[1,],]
  }

  # Convert SpatialLinesDataFrame to SpatialPointsDataFrame
  coast<-as(coast,"SpatialPointsDataFrame")
  
  # Project latitude and longitude to cartesian coordinates for (houses,coast)
  houses_proj <- spTransform(houses, CRS("+init=epsg:27700"))
  coast_proj  <- spTransform(coast, CRS("+init=epsg:27700"))

  # For each house, find distance to and index of nearest coast
  #  1) use nearest neighbor to find nearest point
  #  2) then find distance to nearest two line segments
  #     i.  segment 1:  x to y1
  #     ii. segment 2: x to y2
  #  3) distance is minimum of these
  distance_coast <- get.knnx(coordinates(coast_proj),coordinates(houses_proj),k=1)
  x<-coordinates(coast_proj)[distance_coast$nn.index,]
  f1<-function(i1) {min(i1+1,max(distance_coast$nn.index))}
  i1<-sapply(distance_coast$nn.index,f1)
  y1<-coordinates(coast_proj)[i1,]
  
  f1<-function(i1) {max(i1-1,1)}
  i1<-sapply(distance_coast$nn.index,f1)
  y2<-coordinates(coast_proj)[i1,]

  z<-coordinates(houses_proj)
  # distance to segment 1:   segment1 = lam*y1 + (1-lam)*x
  # compute dist1 = distance(houses_proj,segment1)
  i1<- (y1[,1]!=x[,1] | y1[,2]!=x[,2])
  lam<-rep(0.0,length.out=nrow(houses))
  lam[i1]<- (z[i1,1]-x[i1,1]) * (y1[i1,1]-x[i1,1]) + (z[i1,2]-x[i1,2])*(y1[i1,2]-x[i1,2])
  lam[i1] <- lam[i1] / ( (y1[i1,1]-x[i1,1])^2 + (y1[i1,2]-x[i1,2])^2)
  f1<-function(x) {max(min(x,1),0)}
  lam <- sapply(lam,f1)
  p1 <- lam * y1 + (1-lam) * x
  dist1<-sqrt(rowSums((p1-z)^2))
  
  # distance to segment 2: segment2 = lam*y2 + (1-lam)*x
  # compute dist2 = distance(houses_proj,segment2)
  i1<-(y2[,1]!=x[,1] | y2[,2]!=x[,2])
  lam<-rep(0.0,length.out=nrow(houses))
  lam[i1]<- (z[i1,1]-x[i1,1]) * (y2[i1,1]-x[i1,1]) + (z[i1,2]-x[i1,2])*(y2[i1,2]-x[i1,2])
  lam[i1] <- lam[i1] / ( (y2[i1,1]-x[i1,1])^2 + (y2[i1,2]-x[i1,2])^2)
  f1<-function(x) {max(min(x,1),0)}
  lam <- sapply(lam,f1)
  p2 <- lam * y2 + (1-lam) * x
  dist2<-sqrt(rowSums((p2-z)^2))

  # distance_to_coast = min(dist1,dist2)  
  tempdata<-data.frame(dist1,dist2)
  imin<-apply(tempdata[,1:2],1,which.min)

  mindist<-apply(tempdata,1,min)
  p<-p1
  if (any(imin==2)) {
    p[imin==2,]<-p2[imin==2,]
  }
  
  # Convert back to (longitude,latitude)
  p<-SpatialPoints(p,CRS("+init=epsg:27700"))
  p<-coordinates(spTransform(p, CRS(wgs.84)))

  # predict drive time to new coast  
  newdata<-data.frame(coordinates(houses),p)
  traveltime<-predictTravelTime2(newdata,destname = "temp",region_id,dirs$traveldir)

  # put results in dataframe and give names to each column
  newdata<-cbind(newdata[,3:4],mindist/1000,traveltime$drive_temp)
  colnames(newdata)<-c("coast_lon",
                       "coast_lat",
                       "distance_coast",
                       "drive_coast")
  return(newdata)
}
