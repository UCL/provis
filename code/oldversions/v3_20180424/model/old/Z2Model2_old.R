# Model 2:  Create estimation sample
# Author:   MM,LN
# Version:  2018.02.02

datastub<-"m11"  # "m11"    = domestic
                    # "nondom" = non-domestic
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
if (!exists("region_bbx")) {
  region_bbx<-vector("list",11)
}
wgs.84    <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
ukgrid = "+init=epsg:27700"

for (r in 2:2) {
  region_id<-regnames$region_id[r]    
  region_str<-as.character(regnames$region_str[r]) 
  dirs<-B2SetPath(RootDir,CodeDir,region_id,datastub)
  
  #-----------------------------------------------------------------
  #   Load and clean data
  #-----------------------------------------------------------------
  load(file=paste0(dirs$datadir,"/m2data1.RData"))
  if (N0>0) {
    m2data<-m2data[sample(nrow(m2data),N0),]
  }
 
  if (!exists("m2data$greenbelt")) {
    m2data$greenbelt=0
  } 
  if (is.null(region_bbx[[r]])) {
    # Create bounding box for region
    bbx_str<-paste0("POLYGON((",min(m2data$longitude,na.rm=TRUE)," ",min(m2data$latitude,na.rm=TRUE),",",
                    max(m2data$longitude,na.rm=TRUE)," ",min(m2data$latitude,na.rm=TRUE),",",
                    max(m2data$longitude,na.rm=TRUE)," ",max(m2data$latitude,na.rm=TRUE),",",
                    min(m2data$longitude,na.rm=TRUE)," ",max(m2data$latitude,na.rm=TRUE),",",
                    min(m2data$longitude,na.rm=TRUE)," ",min(m2data$latitude,na.rm=TRUE),"))")
    bbx<-readWKT(bbx_str,p4s=CRS(wgs.84),id=as.character(region_str))
    region_bbx[[r]]<-bbx
  } 

  # clean data
  m2data$imddecile<-factor(m2data$imddecile)

  # 1)  travel times to destinations
  #    drive time : dest$shortlist == TRUE
  #    trans time : dest$raildest  == TRUE
  DestinationFile<-paste0(dirs$datadir,"/destinations",region_id,"B.csv")
  dest<-read.csv(DestinationFile)
  m2data<-predictTravelTime(m2data,dest[dest$shortlist,],region_id,dirs$traveldir,travelmode="drive")
  m2data<-predictTravelTime(m2data,dest[dest$raildest,],region_id,dirs$traveldir,travelmode="trans")

  if (region_id==1) {
    # London train times for CaMKOx
    m2data_new$trans_LON <- with(m2data_new,
    pmin(m2data_new$trans_EUS,
    m2data_new$trans_KGX,
    m2data_new$trans_PAD,
    m2data_new$trans_LIV,
    m2data_new$trans_MAR))
  } else if (region_id==4) {
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
  traveltime_town<-predictTravelTime2(m2data[,c("longitude","latitude","town_lon","town_lat")],
                                      destname = "town",region_id,dirs$traveldir)
  m2data$drive_town<-traveltime_town$drive_town
  
  # 3) Distance to destinations
  m2data<-ComputeNICDistance(m2data,dest)

  # 4) Create distance to coast
  t1<-Sys.time()
  distance<-FindNearestCoastPoint(m2data[,c("longitude","latitude")],region_id,dirs)  
  
  t2<-Sys.time()
  print(paste0("Time used to compute distance to coast: ",round(t2-t1,1)," ",units(t2-t1)))
  m2data$distance_coast<-distance$distance_coast
  m2data$coast_lon<-distance$coast_lon
  m2data$coast_lat<-distance$coast_lat
  m2data$drive_coast<-distance$drive_coast
  rm(distance)
 
  # 5) Distance to AONB
  t1<-Sys.time()
  distance<-FindNearestPark(m2data[,c("longitude","latitude")],
                            parktype="AONB",region_id,dirs)  
  t2<-Sys.time()
  print(paste0("Time used to compute distance to AONB: ",round(t2-t1,1)," ",units(t2-t1)))
  m2data$distance_AONB<-distance$distance_AONB
  m2data$AONB_lon<-distance$AONB_lon
  m2data$AONB_lat<-distance$AONB_lat
  m2data$drive_AONB<-distance$drive_AONB
  m2data$AONB <- distance$AONB   # TRUE if in AONB
  m2data$AONB_ID<-factor(distance$AONB_ID)
  rm(distance)
  
  # 6) Distance to nationalpark
  t1<-Sys.time()
  distance<-FindNearestPark(m2data[,c("longitude","latitude")],
                            parktype="natpark",region_id,dirs)  
  t2<-Sys.time()
  print(paste0("Time used to compute distance to National Park: ",round(t2-t1,1)," ",units(t2-t1)))
  m2data$distance_natpark<-distance$distance_natpark
  m2data$natpark_lon<-distance$natpark_lon
  m2data$natpark_lat<-distance$natpark_lat
  m2data$drive_natpark<-distance$drive_natpark
  m2data$natpark <- distance$natpark   # TRUE if in natpark
  m2data$natpark_ID<-factor(distance$natpark_ID)
  rm(distance)
  
  
  # 7) Distance to motorways
  t1<-Sys.time()
  distance<-FindNearestRoad(m2data[,c("longitude","latitude")],
                            roadtype="motorways",region_id,dirs)
  m2data$distance_motorway<-distance$distance_motorways
  m2data$motorway_lon     <-distance$motorways_lon
  m2data$motorway_lat     <-distance$motorways_lat
  m2data$drive_motorway   <-distance$drive_motorways
  t2<-Sys.time()
  print(paste0("Time used to compute distance to motorway: ",round(t2-t1,1)," ",units(t2-t1)))
  rm(distance)

  # 8) Distance to A-roads
  t1<-Sys.time()
  distance<-FindNearestRoad(m2data[,c("longitude","latitude")],
                            roadtype="aroads",region_id,dirs)
  m2data$distance_aroad<-distance$distance_aroads
  m2data$aroad_lon     <-distance$aroads_lon
  m2data$aroad_lat     <-distance$aroads_lat
  m2data$drive_aroad   <-distance$drive_aroads
  t2<-Sys.time()
  print(paste0("Time used to compute distance to A-road: ",round(t2-t1,1)," ",units(t2-t1)))
  rm(distance)
  
  # 9) Find nearest station and compute travel time
  # colnames(newdata)<-c("station_lon","station_lat","distance_station","drive_station")
  newdata<-FindNearestStation(as.matrix(m2data[,c("longitude","latitude")]),
                               region_id,RootDir,dirs$traveldir)
  m2data$station_lon<-newdata$station_lon
  m2data$station_lat<-newdata$station_lat
  m2data$distance_station<-newdata$distance_station
  m2data$drive_station<-newdata$drive_station
  rm(newdata)
    
  # 10) Create splines
  idrive<-grep("\\<drive_",colnames(m2data))
  itrans<-grep("\\<trans_",colnames(m2data))
  m2dataspline<-list(c(colnames(m2data)[idrive],colnames(m2data)[itrans]))
  names(m2dataspline)[1]<-"varlist"
  m2dataspline$degree<-rep(1,length(m2dataspline$varlist))
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
  
  # 11) create travel time interacted with rank
  # also create splines in these
  m2data<-CreateRank(m2data,"drive")
  m2data<-CreateRank(m2data,"trans")

  # 12) Create splines in  (xdrive_,xtrans_)
  ixdrive<-grep("\\<xdrive_",colnames(m2data))
  ixtrans<-grep("\\<xtrans_",colnames(m2data))
  xspline<-list(c(colnames(m2data)[ixdrive],colnames(m2data)[ixtrans]))
  names(xspline)[1]<-"varlist"
  xspline$degree<-rep(1,length(xspline$varlist))
  xspline$segments <- rep(4,length(xspline$varlist))
  xspline$knots    <- vector("list",length(xspline$varlist))
  
  for (i1 in 1:length(xspline$varlist)) {
    i2<-grep(xspline$varlist[i1],colnames(m2data))
    inonzero<-m2data[,xspline$varlist[i1]]>0
    tempknots<-quantile(m2data[inonzero,xspline$varlist[i1]],
                        seq(0.1,0.9,length.out=xspline$segments[i1]),
                        na.rm=TRUE)
    xspline$knots[[i1]]<-tempknots
    names(xspline$knots)[i1]<-xspline$varlist[i1]
    
    m2data$tempB<-bSpline(m2data[,xspline$varlist[i1]],
                          knots=tempknots,
                          degree=xspline$degree[i1],intercept=FALSE)
    iTempB<-grep("tempB",colnames(m2data))
    names(m2data)[iTempB]<-paste0("spline_",xspline$varlist[i1])
  }
  save(xspline,file=paste0(dirs$outdir,"/xspline.RData"))
  
#-----------------------------------------------------------------
#  Save data
#-----------------------------------------------------------------

save(m2data,file=paste0(dirs$datadir,"/m2data2.RData"))

}
