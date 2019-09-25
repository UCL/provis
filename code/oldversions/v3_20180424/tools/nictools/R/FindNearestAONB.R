#' Find nearest point on AONB to houses and compute travel time
#' @param houses     (n x 2) matrix of coordinates (longitude,latitude)
#' @param region_polygon region polygon (bounding box or convex hull)
#' @param region_id  index for region
#' @param dirs       list of directories: (dirs$mapdir,dirs$traveldir)
#' @return newdata   dataframe with (AONB_lon,AONB_lat,distance_AONB,drive_AONB)
#' @export
#' @examples
#' houses    <- as.matrix(m2data[,c("longitude","latitude")])
#' region_id <- 1
#' dirs$mapdir <- paste0(RootDir,"/data/maps")
#' dirs$traveldir <- paste0(RootDir,"/code/output/TravelTime")
#' newdata<-FindNearestAONB(houses,region_polygon,region_id,dirs)
FindNearestAONB<-function(houses,regino_polygon,region_id,dirs) {

  # define coordinate systems
  wgs.84    <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  ukgrid = "+init=epsg:27700"

  # Load AONB polygon from geo database
  subset(ogrDrivers(), grepl("GDB", name))
  AONBlayer <- ogrListLayers(dirs$AONBdir)
  AONB <- readOGR(dsn=dirs$AONBdir,layer=AONBlayer[[1]])
  AONB <-spTransform(AONB,CRS(wgs.84))

  # Create convex hull of coordinates for current region
  nhouses<-nrow(houses)
  houses<-SpatialPoints(houses,CRS(wgs.84))
  #houses_convexhull<-gConvexHull(houses)

  # Find set of AONB polygons that intersect houses_convexhull
  i1<-gIntersects(AONB,region_polygon,byid=TRUE)

  # Setup Output dataframe with missing values
  newdata<-data.frame(rep(FALSE,nhouses),
                      rep("none",nhouses),
                      rep(NA,nhouses),
                      rep(NA,nhouses),
                      rep(NA,nhouses),
                      rep(NA,nhouses))
  colnames(newdata)<-c("AONB","AONB_ID","distance_AONB","drive_AONB","AONB_lon","AONB_lat")
  if (any(i1)) {
    AONB<-AONB[i1[1,],]
    n1<-sum(i1)

    # Project latitude and longitude to cartesian coordinates for houses
    houses_proj <- spTransform(houses, CRS("+init=epsg:27700"))

    for (i2 in 1:n1) {
      # find all houses that are in each AONB
      temp<-gIntersects(houses,AONB[i2,],byid=TRUE)
      newdata$AONB[temp]          <-TRUE
      newdata$distance_AONB[temp] <- 0.0
      newdata$drive_AONB[temp]    <- 0.0

      # Convert SpatialPolygonsDataFrame to SpatialPointsDataFrame
      AONBlines<-as(AONB[i2,],"SpatialLinesDataFrame")
      AONBpoints<-as(AONBlines,"SpatialPointsDataFrame")

      # Project latitude and longitude to cartesian coordinates for AONB
      AONB_proj  <- spTransform(AONBpoints, CRS("+init=epsg:27700"))

      # For each house, find distance to and index of nearest AONB boundary point
      #  1) use nearest neighbor to find nearest point
      #  2) then find distance to nearest two line segments
      #     i.  segment 1:  x to y1
      #     ii. segment 2: x to y2
      #  3) distance is minimum of these
      distance_AONB <- get.knnx(coordinates(AONB_proj),coordinates(houses_proj),k=1)
      x<-coordinates(AONB_proj)[distance_AONB$nn.index,]
      f1<-function(i1) {min(i1+1,max(distance_AONB$nn.index))}
      i1<-sapply(distance_AONB$nn.index,f1)
      y1<-coordinates(AONB_proj)[i1,]

      f1<-function(i1) {max(i1-1,1)}
      i1<-sapply(distance_AONB$nn.index,f1)
      y2<-coordinates(AONB_proj)[i1,]

      z<-coordinates(houses_proj)
      # distance to segment 1:   segment1 = lam*y1 + (1-lam)*x
      # compute dist1 = distance(houses_proj,segment1)
      i1<- (y1[,1]!=x[,1] | y1[,2]!=x[,2])
      lam<-rep(0.0,length.out=nhouses)
      lam[i1]<- (z[i1,1]-x[i1,1]) * (y1[i1,1]-x[i1,1]) + (z[i1,2]-x[i1,2])*(y1[i1,2]-x[i1,2])
      lam[i1] <- lam[i1] / ( (y1[i1,1]-x[i1,1])^2 + (y1[i1,2]-x[i1,2])^2)
      f1<-function(x) {max(min(x,1),0)}
      lam <- sapply(lam,f1)
      p1 <- lam * y1 + (1-lam) * x
      dist1<-sqrt(rowSums((p1-z)^2))

      # distance to segment 2: segment2 = lam*y2 + (1-lam)*x
      # compute dist2 = distance(houses_proj,segment2)
      i1<-(y2[,1]!=x[,1] | y2[,2]!=x[,2])
      lam<-rep(0.0,length.out=nhouses)
      lam[i1]<- (z[i1,1]-x[i1,1]) * (y2[i1,1]-x[i1,1]) + (z[i1,2]-x[i1,2])*(y2[i1,2]-x[i1,2])
      lam[i1] <- lam[i1] / ( (y2[i1,1]-x[i1,1])^2 + (y2[i1,2]-x[i1,2])^2)
      f1<-function(x) {max(min(x,1),0)}
      lam <- sapply(lam,f1)
      p2 <- lam * y2 + (1-lam) * x
      dist2<-sqrt(rowSums((p2-z)^2))

      # distance_to_AONB = min(dist1,dist2)
      tempdata<-data.frame(dist1,dist2)
      imin<-apply(tempdata[,1:2],1,which.min)

      mindist<-apply(tempdata,1,min)/1000
      p<-p1
      if (any(imin==2)) {
        p[imin==2,]<-p2[imin==2,]
      }

      # Convert back to (longitude,latitude)
      p<-SpatialPoints(p,CRS("+init=epsg:27700"))
      p<-coordinates(spTransform(p, CRS(wgs.84)))

      # predict drive time to new AONB
      traveltime<-predictTravelTime2(data.frame(coordinates(houses),p),
                                     destname = "temp",region_id,dirs$traveldir)

      # put results in dataframe
      #   (p,mindist/1000,drive_temp)
      # temp==FALSE & AONB==FALSE
      if (i2==1) {
        # First AONB polygon
        newdata$distance_AONB[!temp]<- mindist[!temp]
        newdata[!temp,c("AONB_lon","AONB_lat")]<-p[!temp,]
        newdata$drive_AONB[!temp]<-traveltime$drive_temp[!temp]
        newdata$AONB_ID <- as.character(AONB@data$NAME[i2])
      } else {
        # (AONB==FALSE and temp==FALSE & mindist/1000 < distance_AONB)
        itemp<- (!newdata$AONB & (mindist <newdata$distance_AONB))
        if (any(itemp)) {
          newdata$distance_AONB[itemp]<-mindist[itemp]
          newdata[itemp,c("AONB_lon","AONB_lat")]<-p[itemp,]
          newdata$AONB_ID[itemp]<- as.character(AONB@data$NAME[i2])
        }
      }
    }
  }
  return(newdata)
}
