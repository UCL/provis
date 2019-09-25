#' A4CreateSettlement(region_id,dirs,filename,title,outfile,m2data_new)
#' Create data frames for new settlement and predict prices
#' 
#' @param region_id       id number for region
#' @param dirs            list of directories
#' @param filename        .csv file containing details of settlement
#' @param title           title of table of summary stats
#' @param outfile         output file
#' @param m2data_new      input base dataframe for new settlement
#' @return newdata        list containing (m1data_new,m2data_new)
#' @keywords new settlement
#' @export
#' @examples
#' newdata <- A4CreateSettlement(region_id = 1,dirs,filename = "Bedford.csv",title,outfile)
#' newdata <- A4CreateSettlement(region_id = 1,dirs,m2data_new = m2data_base)
A4CreateSettlement<-function(region_id,dirs,filename,title,outfile,m2data_new) {
  default_warn<-getOption("warn")
  options(warn=-1)

  # proj_4string for spatial objects
  wgs.84    <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

  if (!missing(filename)) {
    # Load New settlement data
  #  example:  filename<-"Bedford.csv"  
    m2data_new<-read.csv(paste0(dirs$newdatadir,"/",filename), header=TRUE)
  } else if (missing(filename) & missing(m2data_new)) {
    stop("Error in A4CreateSettlement. Must input either 'filename' or 'm2data_new'.")
  }
  DestinationFile <- paste0(dirs$datadir,"/destinations",region_id,"B.csv")
  dest            <- read.csv(DestinationFile)
  
  load(paste0(dirs$newdatadir,"/m1data_template.RData"))
  load(paste0(dirs$datadir,"/m2data2.RData"))

  levels(m2data_new$tenure)<-c("F","L")

  # create dataset to estimate model 1
  if (nrow(m1data_template)>nrow(m2data_new)) {
    m1data_new<-m1data_template[1:nrow(m2data_new),]
  } else {
    m1data_new<-m1data_template[sample(c(1:nrow(m1data_template)),nrow(m2data_new),replace=TRUE),]  
  }  
  m1data_new$date             <- as.Date("2017-12-31")
  m1data_new$year             <- factor("2017",levels=levels(m1data_new$year))
  m1data_new$propertytype     <- m2data_new$propertytype
  m1data_new$newbuild         <- factor("Y",levels=levels(m1data_template$newbuild))
  m1data_new$tenure           <- m2data_new$tenure
  m1data_new$total_floor_area <- m2data_new$total_floor_area
  m1data_new$latitude         <- m2data_new$latitude
  m1data_new$longitude        <- m2data_new$longitude
  
  # create interaction (lat_long)
  m1data_new               <- cbind(m1data_new,m1data_new$latitude*m1data_new$longitude)
  i1                       <- grep("m1data*",colnames(m1data_new))
  colnames(m1data_new)[i1] <- "lat_long"
  
  # Restricted land percentage
  m2data_new$restrictedland_pct<-mean(tempdata$restrictedland_pct)

  #  predict location_value for new settlement
  #    iold = indexes of points in m2data that are in the region defined by envelope of m2data_new
  p1<-SpatialPoints(m2data_new[,c("longitude","latitude")],CRS(wgs.84))
  p2<-SpatialPoints(m2data[,c("longitude","latitude")],CRS(wgs.84))
  
  poly1<-gEnvelope(p1)
  poly2<-gBuffer(poly1,width=0.1)
  
  iold<-gIntersects(poly1,p2,byid=TRUE)
  if (sum(iold)<100) {
      iold<-gIntersects(poly2,p2,byid=TRUE)
  }
  if (sum(iold)<100) {
      laname<-droplevels(m2data_new$laname)
      iold <- rep(FALSE,length=nrow(m2data))
      for (i1 in levels(laname)) {
          itemp <- levels(m2data$laname)[m2data$laname]==i1
          iold[itemp]<-TRUE
      }
  }
  tempdata<-m2data[iold[,1],]
  if (nrow(tempdata)>1000) {
      isample<-sample(c(1:nrow(tempdata)),size=1000,replace=F)
  }else {
      isample<-c(1:nrow(tempdata))
  }
  
  np_location_value         <-npreg(location_value~latitude+longitude,data=tempdata[isample,])
  m2data_new$location_value <-predict(np_location_value,newdata=m2data_new)

  # 1) imdrank
  # 2) imddecile
  # 3) prob_4band
  # 4) noiseclass
  # 5) roadnoise
  # 6) greenbelt
  i1<-sample(c(1:sum(iold)),size=nrow(m2data_new),replace=TRUE)
  m2data_new$imdrank<-tempdata$imdrank[i1]
  m2data_new$imddecile<-tempdata$imddecile[i1]
  m2data_new$prob_4band<-tempdata$prob_4band[i1]
  m2data_new$noiseclass<-tempdata$noiseclass[i1]
  m2data_new$roadnoise<-tempdata$roadnoise[i1]
  m2data_new$greenbelt<-tempdata$greenbelt[i1]

  # 1) Estimate travel times
  m2data_new<-predictTravelTime(m2data_new,dest[dest$shortlist,],region_id,dirs$traveldir,
                                travelmode="drive",drop_outlier=FALSE)
  m2data_new<-predictTravelTime(m2data_new,dest[dest$raildest,],region_id,dirs$traveldir,
                                travelmode="trans",drop_outlier=FALSE)
  
  if (region_id==1) {
    # London train times for CaMKOx
    m2data_new$trans_LON <- with(m2data_new,
                                 pmin(m2data_new$trans_EUS,
                                      m2data_new$trans_KGX,
                                      m2data_new$trans_PAD,
                                      m2data_new$trans_LIV,
                                      m2data_new$trans_MAR))
  } else if (region_id==4) {
      # London
      m2data$trans_LON <- with(m2data,
      pmin(m2data$trans_EUS,m2data$trans_KGX,
      m2data$trans_LIV,
      m2data$trans_MAR))
      m2data$distance_airport<-with(m2data,
      pmin(m2data$distance_STD,
      m2data$distance_LUT_AIR,
      m2data$distance_SEN_AIR,
      m2data$distance_NWH_AIR))
  }
  
  # 2) Travel time to postal_town
  traveltime_town<-predictTravelTime2(m2data_new[,c("longitude","latitude","town_lon","town_lat")],
                                      destname = "town",region_id,dirs$traveldir)
  m2data$drive_town<-traveltime_town$drive_town

  # 3) Distance to destinations
  m2data_new<-ComputeNICDistance(m2data_new,dest) 

  # 4) distance to coast
  distance<-FindNearestCoastPoint(m2data_new[,c("longitude","latitude")],region_id,dirs)
  m2data_new$distance_coast <- distance$distance_coast
  m2data_new$coast_lon      <- distance$coast_lon
  m2data_new$coast_lat      <- distance$coast_lat
  m2data_new$drive_coast    <- distance$drive_coast
  rm(distance)

  # Find nearest station and compute travel time
  # colnames(newdata)<-c("station_lon","station_lat","distance_station","drive_station")
  newdata<-FindNearestStation(as.matrix(m2data_new[,c("longitude","latitude")]),
                              region_id,dirs$rootdir,dirs$traveldir)
  m2data_new$station_lon<-newdata$station_lon
  m2data_new$station_lat<-newdata$station_lat
  m2data_new$distance_station<-newdata$distance_station
  m2data_new$drive_station<-newdata$drive_station
  rm(newdata)
  
  # Compute splines
  load(file=paste0(dirs$outdir,"/m2dataspline.RData"))
  for (i1 in 1:length(m2dataspline$varlist)) {
    i2<-grep(m2dataspline$varlist[i1],colnames(m2data_new))
    tempknots<-m2dataspline$knots[[i1]]
    m2data_new$tempB<-bSpline(m2data_new[,m2dataspline$varlist[i1]],
                              knots=tempknots,
                              degree=m2dataspline$degree[i1],intercept=FALSE)
    iTempB<-grep("tempB",colnames(m2data_new))
    names(m2data_new)[iTempB]<-paste0("spline_",m2dataspline$varlist[i1])
  }
  
  # summary statistics
  if (!missing("outfile")) {
    stargazer(tempdata[,m2ols0$varlist],
              m2data_new[,m2ols0$varlist],title=title,
              align=TRUE, out.header=TRUE,digits=4,
              out=paste0(dirs$outdir,"/",outfile))
  }
  load(file=paste0(dirs$datadir,"/m1data.RData"))
  if (nrow(m1data)>nrow(m1data_new)) {
    # Extract subset of m1data same size as m1data_new
    n1<-nrow(m1data)
    n2<-nrow(m1data_new)
    set.seed(569472)
    i1<-sample(c(1:n1),size=n2)
    m1data<-m1data[i1,]
  } else if (nrow(m1data_new)>nrow(m1data)) {
    # m1data_new has more rows than m1data
    # create sample (with replacement) from m1data and 
    # set size equal to size of m1data_new
    n1<=nrow(m1data_new)
    n2<-nrow(m1data)
    set.seed(569471)
    i1<-sample(c(1:n2),size=n1,replace=TRUE)
    m1data<-m1data[i1,]
  }
  
  outdata<-list(m1data_new,m2data_new,m1data)
  names(outdata)<-c("m1data_new","m2data_new","m1data_old")
  options(warn=default_warn)
  return(outdata)
}
