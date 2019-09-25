#' Create new travel time variables and append to dataset
#' @param m2data        dataset
#' @param dest          matrix: destinations
#' @param region_polygon (convexhull or bounding box)
#' @param region        id for region
#' @param dir           list of directories
#' @param drop_outlier  TRUE = drop outliers when predicting travel times
#' @param basis         c("glp","tensor","cheb") basis for travel time model
#' @return m2data       dataset with new variables added
#' @examples
#' m2data<-A2Model2_newvariables(m2data,dest,region_polygon,region,dirs,drop_outlier=TRUE,basis="cheb")
A2Model2_newvariables<-function(m2data,dest,region_polygon,region,dirs,drop_outlier,basis="glp") {
  m2data<-predictTravelTime(m2data,dest[dest$shortlist,],region,dirs$traveldir,travelmode="drive",
                            drop_outlier=drop_outlier,basis)
  m2data<-predictTravelTime(m2data,dest[dest$raildest,],region,dirs$traveldir,travelmode="trans",
                            drop_outlier=drop_outlier,basis)

  if (region==1) {
    # London train times for CaMKOx
    m2data$trans_LON <- with(m2data,
                             pmin(m2data$trans_EUS,
                                  m2data$trans_KGX,
                                  m2data$trans_STP,
                                  m2data$trans_PAD,
                                  m2data$trans_LIV,
                                  m2data$trans_MAR))
    i1<-grepl("trans_EUS",colnames(m2data)) |
      grepl("trans_KGX",colnames(m2data)) |
      grepl("trans_STP",colnames(m2data)) |
      grepl("trans_PAD",colnames(m2data)) |
      grepl("trans_LIV",colnames(m2data)) |
      grepl("trans_MAR",colnames(m2data))
    i2 <-c(1:ncol(m2data))[i1]  
    m2data<-m2data[,-i2]
    m2data$drive_LON <- with(m2data,
                             pmin(m2data$drive_LIV,
                                  m2data$drive_KGX,
                                  m2data$drive_STP,
                                  m2data$drive_EUS,
                                  m2data$drive_MAR,
                                  m2data$drive_PAD))
    i1<-grepl("drive_LIV",colnames(m2data)) |
        grepl("drive_KGX",colnames(m2data)) |
        grepl("drive_STP",colnames(m2data)) |
        grepl("drive_EUS",colnames(m2data)) |
        grepl("drive_MAR",colnames(m2data)) |
        grepl("drive_PAD",colnames(m2data))
    i2 <-c(1:ncol(m2data))[i1]  
    m2data<-m2data[,-i2]
    
  } else if (region==4) {
    m2data$trans_LON <- with(m2data,
                             pmin(m2data$trans_EUS,
                                  m2data$trans_KGX,
                                  m2data$trans_STP,
                                  m2data$trans_LIV,
                                  m2data$trans_MAR))
    i1<-grepl("trans_EUS",colnames(m2data)) |
        grepl("trans_KGX",colnames(m2data)) |
        grepl("trans_STP",colnames(m2data)) |
        grepl("trans_LIV",colnames(m2data)) |
        grepl("trans_MAR",colnames(m2data))
    i2 <-c(1:ncol(m2data))[i1]  
    m2data<-m2data[,-i2]
    
    m2data$drive_LON <- with(m2data,
                             pmin(m2data$drive_EUS,
                                  m2data$drive_KGX,
                                  m2data$drive_STP,
                                  m2data$drive_LIV,
                                  m2data$drive_MAR))
    i1<-grepl("drive_EUS",colnames(m2data)) |
      grepl("drive_KGX",colnames(m2data)) |
      grepl("drive_STP",colnames(m2data)) |
      grepl("drive_LIV",colnames(m2data)) |
      grepl("drive_MAR",colnames(m2data))
    i2 <-c(1:ncol(m2data))[i1]  
    m2data<-m2data[,-i2]
    
  } else if (region==5) {
    # London

    m2data$trans_LON <- with(m2data,
                             pmin(m2data$trans_CHX,
                                  m2data$trans_EUS,
                                  m2data$trans_KGX,
                                  m2data$trans_LIV,
                                  m2data$trans_LBG,
                                  m2data$trans_MYB,
                                  m2data$trans_PAD,
                                  m2data$trans_STP,
                                  m2data$trans_WAT))
    i1<-grepl("trans_CHX",colnames(m2data)) |
        grepl("trans_EUS",colnames(m2data)) |
        grepl("trans_KGX",colnames(m2data)) |
        grepl("trans_LIV",colnames(m2data)) |
        grepl("trans_LBG",colnames(m2data)) |
        grepl("trans_MYB",colnames(m2data)) |
        grepl("trans_PAD",colnames(m2data)) |
        grepl("trans_STP",colnames(m2data)) |
        grepl("trans_WAT",colnames(m2data)) 
    i2 <-c(1:ncol(m2data))[i1]  
    m2data<-m2data[,-i2]
    m2data$drive_LON <- with(m2data,
                             pmin(m2data$drive_EUS,
                                  m2data$drive_CHX,
                                  m2data$drive_EUS,
                                  m2data$drive_KGX,
                                  m2data$drive_LIV,
                                  m2data$drive_LBG,
                                  m2data$drive_MYB,
                                  m2data$drive_PAD,
                                  m2data$drive_STP,
                                  m2data$drive_WAT))
    i1<-grepl("drive_CHX",colnames(m2data)) |
        grepl("drive_EUS",colnames(m2data)) |
        grepl("drive_KGX",colnames(m2data)) |
        grepl("drive_LIV",colnames(m2data)) |
        grepl("drive_LBG",colnames(m2data)) |
        grepl("drive_MYB",colnames(m2data)) |
        grepl("drive_PAD",colnames(m2data)) |
        grepl("drive_STP",colnames(m2data)) |
        grepl("drive_WAT",colnames(m2data)) 
      
    i2 <-c(1:ncol(m2data))[i1]  
    m2data<-m2data[,-i2]
  } else if (region==8) {
    # southeast
    m2data$trans_LON <- with(m2data,
                             pmin(m2data$trans_EUS,
                                  m2data$trans_VIC,
                                  m2data$trans_LBR,
                                  m2data$trans_WAT,
                                  m2data$trans_PAD,
                                  m2data$trans_MAR))
    m2data$drive_LON <- with(m2data,
                             pmin(m2data$drive_EUS,
                                  m2data$drive_VIC,
                                  m2data$drive_LBR,
                                  m2data$drive_WAT,
                                  m2data$drive_PAD,
                                  m2data$drive_MAR))
    i1<-grepl("trans_EUS",colnames(m2data)) |
        grepl("trans_VIC",colnames(m2data)) |
        grepl("trans_LBR",colnames(m2data)) |
        grepl("trans_WAT",colnames(m2data)) |
        grepl("trans_PAD",colnames(m2data)) |
        grepl("trans_MAR",colnames(m2data))
    i2 <-c(1:ncol(m2data))[i1]  
    m2data<-m2data[,-i2]
    i1<-grepl("drive_EUS",colnames(m2data)) |
        grepl("drive_VIC",colnames(m2data)) |
        grepl("drive_LBR",colnames(m2data)) |
        grepl("drive_WAT",colnames(m2data)) |
        grepl("drive_PAD",colnames(m2data)) |
        grepl("drive_MAR",colnames(m2data))
    i2 <-c(1:ncol(m2data))[i1]  
    m2data<-m2data[,-i2]
  }

  # 2) Travel time to postal_town
  traveltime_town<-predictTravelTime2(m2data[,c("longitude","latitude","town_lon","town_lat")],
                                      destname = "town",region,dirs$traveldir,printstr="town",
                                      basis=basis)
  m2data$drive_town<-traveltime_town$drive_town
  m2data$trans_town<-traveltime_town$trans_town
    
  # 3) Distance to destinations
  m2data<-ComputeNICDistance(m2data,dest)
  airports<-dest$shortname[dest$airport]
  m2data$distance_airport<-10000
  for (i1 in airports) {
    m2data$distance_airport<- pmin(m2data$distance_airport,
                                   m2data[,paste0("distance_",i1)])
    
  }
  
  # 4) Create distance to coast
  #     omit for London and CAMKOX
  if (region!=1 & region!=5) {
    distance<-FindNearestCoastPoint(m2data[,c("longitude","latitude")],region_polygon,region,dirs,
                                    basis)
    m2data$distance_coast<-distance$distance_coast
    m2data$coast_lon<-distance$coast_lon
    m2data$coast_lat<-distance$coast_lat
    m2data$drive_coast<-distance$drive_coast
    m2data$trans_coast<-distance$trans_coast
    rm(distance)
  }

  # 5) Distance to AONB
  if (region!=5) {
    distance<-FindNearestPark(m2data[,c("longitude","latitude")],
                              region_polygon,parktype="AONB",region,dirs,basis)
    m2data$distance_AONB<-distance$distance_AONB
    m2data$AONB_lon<-distance$AONB_lon
    m2data$AONB_lat<-distance$AONB_lat
    m2data$drive_AONB<-distance$drive_AONB
    m2data$AONB <- distance$AONB   # TRUE if in AONB
    m2data$AONB_ID<-factor(distance$AONB_ID)
    rm(distance)
  }
  # 6) Distance to nationalpark
  #    omit for London and CAMKOX
  browser()
  if (region!=1 & region!=5) {
    distance<-FindNearestPark(m2data[,c("longitude","latitude")],
                              region_polygon,parktype="natpark",region,dirs,basis)
    m2data$distance_natpark<-distance$distance_natpark
    m2data$natpark_lon<-distance$natpark_lon  
    m2data$natpark_lat<-distance$natpark_lat  
    m2data$drive_natpark<-distance$drive_natpark
    m2data$natpark <- distance$natpark   # TRUE if in natpark
    m2data$natpark_ID<-factor(distance$natpark_ID)
    rm(distance)
  }

  # 7) Distance to motorways
  distance<-FindNearestRoad(m2data[,c("longitude","latitude")],
                            region_polygon,roadtype="motorways",region,dirs,basis)
  m2data$distance_motorway<-distance$distance_motorways
  m2data$motorway_lon     <-distance$motorways_lon
  m2data$motorway_lat     <-distance$motorways_lat
  m2data$drive_motorway   <-distance$drive_motorways
  rm(distance)

  # 8) Distance to A-roads
  distance<-FindNearestRoad(m2data[,c("longitude","latitude")],
                            region_polygon,roadtype="aroads",region,dirs,basis)
  m2data$distance_aroad<-distance$distance_aroads
  m2data$aroad_lon     <-distance$aroads_lon
  m2data$aroad_lat     <-distance$aroads_lat
  m2data$drive_aroad   <-distance$drive_aroads
  rm(distance)

  # 9) Find nearest station and compute travel time
  newdata<-FindNearestStation(as.matrix(m2data[,c("longitude","latitude")]),
                              region,RootDir,dirs$traveldir,basis,stationtype="rail")
  newdata<-FindNearestStation(as.matrix(m2data[,c("longitude","latitude")]),region,RootDir,dirs$traveldir,basis,stationtype="rail")
  
  m2data$station_lon<-newdata$station_lon
  m2data$station_lat<-newdata$station_lat
  m2data$distance_station<-newdata$distance_station
  m2data$drive_station<-newdata$drive_station
  m2data$trans_station<-newdata$trans_station
  rm(newdata)
  
  # distance to tube station for london
  if (region==5) {
    newdata<-FindNearestStation(as.matrix(m2data[,c("longitude","latitude")]),
                                region,RootDir,dirs$traveldir,basis,stationtype="tube")
    m2data$tubestation_lon<-newdata$tubestation_lon
    m2data$tubestation_lat<-newdata$tubestation_lat
    m2data$distance_tubestation<-newdata$distance_tubestation
    m2data$drive_tubestation<-newdata$drive_tubestation
    m2data$trans_tubestation<-newdata$trans_tubestation
    rm(newdata)
  }
  m2data<-ComputeAverageTime(m2data)
  
  # 10) for each property, rank destinations in terms of traveltime
  #     SKIP THIS STEP FOR LONDON
  if (region!=5) {
    m2data<-CreateRank(m2data,"drive")
    m2data<-CreateRank(m2data,"trans")
    m2data<-CreateRank(m2data,"avgtime")
  }
  return(m2data)
}
