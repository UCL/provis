# Model 2:  Create estimation sample
# Author:   MM,LN
# Version:  2018.01.01

LondonFlag<-0   # 0 : Only Greater London
# 1 : London + some adjacent counties  

N0<-10000      # N0   = size of fullsample used for estimation
               # N0   = 0 then use all data (excluding outliers)

nregs<-11
regnames<-data.frame(region_id=seq(1:nregs), 
                     region_str=c("CaMKOx", "CornwallDevon", 
                                  "EastMid", "EastEng", 
                                  "London", "NE", "NW", 
                                  "SE", "SW", "WestMid", 
                                  "YorkshireHumber"))
for (r in c(1:4,6:11)) {
  region_id<-regnames$region_id[r]    
  region_str<-as.character(regnames$region_str[r]) 
  dirs<-B2SetPath(RootDir,CodeDir,region_id)
  
  #-----------------------------------------------------------------
  #   Load and clean data
  #-----------------------------------------------------------------
  load(file=paste0(dirs$datadir,"/m2data1.RData"))
  if (N0>0 & N0<nrow(m2data)) {
    m2data<-m2data[sample(nrow(m2data),N0),]
  }
  
  # clean data
  m2data$imddecile<-factor(m2data$imddecile)

  # Make predictions: travel times to destinations
  #   only predict travel times to destinations with shortlist=TRUE
  DestinationFile<-paste0(dirs$datadir,"/destinations",region_id,".csv")
  dest<-read.csv(DestinationFile)
  m2data<-predictTravelTime(m2data,dest[dest$shortlist,],region_id,dirs$traveldir,travelmode="drive")
  m2data<-predictTravelTime(m2data,dest[dest$shortlist,],region_id,dirs$traveldir,travelmode="trans")
  
  n<-ncol(m2data)
  ilat<-grep("latitude",colnames(m2data))
  ilon<-grep("longitude",colnames(m2data))
  for (i1 in 1:nrow(dest)) { 
    n<-n+1
    assign("x",distGeo(m2data[,c(ilon,ilat)],dest[i1,c("longitude","latitude")])/1000)
    m2data$tempx<-x
    names(m2data)[n]<-paste0("distance_",dest$shortname[i1])
  }

  # Create distance to coast
  distance<-FindNearestCoastPoint(m2data[,c("longitude","latitude")],region_id,dirs)  
  m2data$distance_coast<-distance$distance_coast
  m2data$coast_lon<-distance$coast_lon
  m2data$coast_lat<-distance$coast_lat
  m2data$drive_coast<-distance$drive_coast
  rm(distance)
  
  # Find nearest station and compute travel time
  # colnames(newdata)<-c("station_lon","station_lat","distance_station","drive_station")
  newdata<-FindNearestStation(as.matrix(m2data[,c("longitude","latitude")]),
                               region_id,RootDir,dirs$traveldir)
  m2data$station_lon<-newdata$station_lon
  m2data$station_lat<-newdata$station_lat
  m2data$distance_station<-newdata$distance_station
  m2data$drive_station<-newdata$drive_station
  rm(newdata)
    
  # Create splines
  idrive<-grep("drive_",colnames(m2data))
  itrans<-grep("trans_",colnames(m2data))
  m2dataspline<-list(c(colnames(m2data)[idrive],colnames(m2data)[itrans]))
  names(m2dataspline)[1]<-"varlist"
  m2dataspline$degree<-rep(3,length(m2dataspline$varlist))
  m2dataspline$segments <- rep(4,length(m2dataspline$varlist))
  m2dataspline$knots    <- vector("list",length(m2dataspline$varlist))
  
  for (i1 in 1:length(m2dataspline$varlist)) {
    i2<-grep(m2dataspline$varlist[i1],colnames(m2data))
    tempknots<-quantile(m2data[,m2dataspline$varlist[i1]],
                        seq(0.1,0.9,length.out=m2dataspline$segments[i1]),
                        na.rm=TRUE)
    m2dataspline$knots[[i1]]<-tempknots
    names(m2dataspline$knots)[i1]<-m2dataspline$varlist[i1]
  
    m2data$tempB<-bSpline(m2data[,m2dataspline$varlist[i1]],
                          knots=tempknots,
                        degree=m2dataspline$degree[i1],intercept=FALSE)
    iTempB<-grep("tempB",colnames(m2data))
    names(m2data)[iTempB]<-paste0("spline_",m2dataspline$varlist[i1])
  }
  save(m2dataspline,file=paste0(dirs$outdir,"/m2dataspline.RData"))
#-----------------------------------------------------------------
#  Save data
#-----------------------------------------------------------------

save(m2data,file=paste0(dirs$datadir,"/m2data2.RData"))

}
