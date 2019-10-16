#' Find nearest point on park (AONB,NationalPark) to houses and compute travel time
#' @param houses     (n x 2) matrix of coordinates (longitude,latitude)
#' @param region_polygon polygon for region (bounding box or convex hull)
#' @param parktype   c("AONB","natpark")
#' @param region_id  index for region
#' @param dirs       list of directories: (dirs$mapdir,dirs$traveldir)
#' @param basis      c("glp","tensor","cheb") basis for travel time model
#' @return newdata   dataframe with (xxx_lon,xxx_lat,distance_xxx,drive_xxx)
#' @export
#' @examples
#' houses    <- as.matrix(m2data[,c("longitude","latitude")])
#' region_id <- 1
#' dirs$mapdir <- paste0(RootDir,"/data/maps")
#' dirs$traveldir <- paste0(RootDir,"/code/output/TravelTime")
#' newdata<-FindNearestPark(houses,region_polygon,parktype="AONB",region_id,dirs)
#' newdata<-FindNearestPark(houses,region_polygon,parktype="natpark",region_id,dirs)
FindNearestPark<-function(houses,region_polygon,parktype="AONB",region_id,dirs,basis="glp") {

  # define coordinate systems
  wgs.84    <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  ukgrid = "+init=epsg:27700"
  if (parktype=="AONB") {
    # Load AONB polygon from geo database
    subset(ogrDrivers(), grepl("GDB", name))
    parklayer <- ogrListLayers(dirs$AONBdir)
    park <- readOGR(dsn=dirs$AONBdir,layer=parklayer[[1]])
  }  else if (parktype=="natpark") {
    parkfile<-paste0(dirs$parkdir,"/National_Parks_England.kml")
    parklayer<-ogrListLayers(parkfile)
    park<-readOGR(parkfile,layer=parklayer)
  }

  park <-spTransform(park,CRS(wgs.84))

  # Create convex hull of coordinates for current region
  nhouses<-nrow(houses)
  houses<-SpatialPoints(houses,CRS(wgs.84))
#  houses_convexhull<-gConvexHull(houses)

  # Find set of park polygons that intersect houses_convexhull
  i1<-gIntersects(park,region_polygon,byid=TRUE)

  # Setup Output dataframe with missing values
  newdata<-data.frame(rep(FALSE,nhouses),
                      rep("none",nhouses),
                      rep(NA,nhouses),
                      rep(NA,nhouses),
                      rep(NA,nhouses),
                      rep(NA,nhouses))
  colnames(newdata)<-c("park",
                       "park_ID",
                       "distance_park",
                       "drive_park",
                       "park_lon",
                       "park_lat")
  if (any(i1)) {
    park<-park[i1[1,],]
    n1<-sum(i1)

    # Project latitude and longitude to cartesian coordinates for houses
    houses_proj <- spTransform(houses, CRS("+init=epsg:27700"))

    for (i2 in 1:n1) {
      # find all houses that are in each AONB
#      browser()
      temp<-try(gIntersects(houses,park[i2,],byid=TRUE),silent=TRUE)
      if (class(temp)=="try-error") {
        ptemp<-park[i2,]
        ptemp@polygons[[1]]<-checkPolygonsHoles(ptemp@polygons[[1]])
        temp<-gIntersects(houses,ptemp,byid=TRUE)
      }
      newdata$park[temp]          <-TRUE
      newdata$distance_park[temp] <- 0.0
      newdata$drive_park[temp]    <- 0.0

      # Convert SpatialPolygonsDataFrame to SpatialPointsDataFrame
      parklines<-as(park[i2,],"SpatialLinesDataFrame")
      parkpoints<-as(parklines,"SpatialPointsDataFrame")

      # Project latitude and longitude to cartesian coordinates for park
      park_proj  <- spTransform(parkpoints, CRS("+init=epsg:27700"))

      # For each house, find distance to and index of nearest park boundary point
      #  1) use nearest neighbor to find nearest point
      #  2) then find distance to nearest two line segments
      #     i.  segment 1:  x to y1
      #     ii. segment 2: x to y2
      #  3) distance is minimum of these
      distance_park <- get.knnx(coordinates(park_proj),coordinates(houses_proj),k=1)
      x<-coordinates(park_proj)[distance_park$nn.index,]
      f1<-function(i1) {min(i1+1,max(distance_park$nn.index))}
      i1<-sapply(distance_park$nn.index,f1)
      y1<-coordinates(park_proj)[i1,]

      f1<-function(i1) {max(i1-1,1)}
      i1<-sapply(distance_park$nn.index,f1)
      y2<-coordinates(park_proj)[i1,]

      z<-coordinates(houses_proj)
      # distance to segment 1:   segment1 = lam*y1 + (1-lam)*x
      # compute dist1 = distance(houses_proj,segment1)
      i1<- (distance_park$nn.index<max(distance_park$nn.index))
      lam<-rep(0.0,length.out=nhouses)
      if (any(i1)) {
        lam[i1]<- (z[i1,1]-x[i1,1]) * (y1[i1,1]-x[i1,1]) + (z[i1,2]-x[i1,2])*(y1[i1,2]-x[i1,2])
        lam[i1] <- lam[i1] / ( (y1[i1,1]-x[i1,1])^2 + (y1[i1,2]-x[i1,2])^2)
        f1<-function(x) {max(min(x,1),0)}
        lam[i1] <- sapply(lam[i1],f1)
      }
      p1 <- lam * y1 + (1-lam) * x
      dist1<-sqrt(rowSums((p1-z)^2))

      # distance to segment 2: segment2 = lam*y2 + (1-lam)*x
      # compute dist2 = distance(houses_proj,segment2)
      i1<-(distance_park$nn.index>1)
      lam<-rep(0.0,length.out=nhouses)
      if (any(i1)) {
        lam[i1]<- (z[i1,1]-x[i1,1]) * (y2[i1,1]-x[i1,1]) + (z[i1,2]-x[i1,2])*(y2[i1,2]-x[i1,2])
        lam[i1] <- lam[i1] / ( (y2[i1,1]-x[i1,1])^2 + (y2[i1,2]-x[i1,2])^2)
        f1<-function(x) {max(min(x,1),0)}
        lam[i1] <- sapply(lam[i1],f1)
      }
      p2 <- lam * y2 + (1-lam) * x
      dist2<-sqrt(rowSums((p2-z)^2))

      # distance_to_park = min(dist1,dist2)
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
      if (parktype=="AONB") {
        printstr<-paste0(as.character(park@data$NAME[i2])," AONB")
      } else {
        printstr<-paste0(as.character(park@data$Name[i2])," Nat. Park")
      }
      traveltime<-predictTravelTime2(data.frame(coordinates(houses),p),
                                     destname = "temp",region_id,dirs$traveldir,
                                     printstr = printstr,basis)

      # put results in dataframe
      #   (p,mindist/1000,drive_temp)
      # temp==FALSE & AONB==FALSE
      if (i2==1) {
        # First AONB polygon
        newdata$distance_park[!temp]<- mindist[!temp]
        newdata[!temp,c("park_lon","park_lat")]<-p[!temp,]
        newdata$drive_park[!temp]<-traveltime$drive_temp[!temp]
        if (parktype=="AONB") {
          newdata$park_ID <- as.character(park@data$NAME[i2])
        } else if (parktype=="natpark") {
          newdata$park_ID <- as.character(park@data$Name[i2])
        }
      } else {
        # (park==FALSE and temp==FALSE & mindist/1000 < distance_park)
        itemp<- (!newdata$park & (mindist <newdata$distance_park))
        if (any(itemp)) {
          newdata$distance_park[itemp]<-mindist[itemp]
          newdata[itemp,c("park_lon","park_lat")]<-p[itemp,]
          if (parktype=="AONB") {
            newdata$park_ID[itemp]<- as.character(park@data$NAME[i2])
          } else if (parktype=="natpark") {
            newdata$park_ID[itemp]<- as.character(park@data$Name[i2])
          }
        }
      }
    }
  }
  colnames(newdata)<-c(parktype,
                       paste0(parktype,"_ID"),
                       paste0("distance_",parktype),
                       paste0("drive_",parktype),
                       paste0(parktype,"_lon"),
                       paste0(parktype,"_lat"))
  return(newdata)
}
