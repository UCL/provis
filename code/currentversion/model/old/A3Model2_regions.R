# Estimate model 2
# Author:  MM,LN
# Version: 2018.01.01

# Set RootDir
if (!exists("RootDir") ) {
  host<-system("hostname",intern=TRUE)
  if (host=="dh-230-mac.econ.ucl.ac.uk") {
    RootDir<-"/Users/uctpln0/Documents/research/hedonic/NIC"
  } else if (host=="minelava") {
    RootDir<-"C:/a/research/hedonic/NIC"
  } else if (host=="jake.local" | host=="vic.local") {
    RootDir<-"/home/uctpln0/hedonic/NIC" 
  }
    else if (host=="DH-G06-03") {
      RootDir<-"U:/NICProject"
  } else if (host=="MateuszsMacBook") {
    RootDir<-"/Users/mateuszmysliwski/Dropbox/NICProject"
  } else {
    info_sys<-Sys.info()
    user<-info_sys["user"]
    if (user=="uctpln0") {
      RootDir<-"/home/uctpln0/hedonic/NIC"
    } else {
      RootDir<-"/Users/larsnesheim/Documents/research/hedonic/NIC"
    }
  }
}
#------------------------------------------------------------
source(paste0(RootDir,"/code/model/B1LoadLibrary.R"))
source(paste0(RootDir,"/code/model/B2SetPath.R"))
source(paste0(RootDir,"/code/model/B4GetVarList.R"))

LondonFlag<-1   # 0 : Only Greater London
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
for (r in 1:nregs) {
  region_id<-regnames$region_id[r]    
  region_str<-as.character(regnames$region_str[r]) 
  dirs<-B2SetPath(RootDir,region_id)
  
  # Load data
  load(file=paste0(dirs$datadir,"/m2data2.RData"))
  attach(m2data)

  # Define subsamples
  qnt <- quantile(location_value, probs=c(.01, .99),na.rm=TRUE)
  iFull <- location_value>qnt[1] & location_value<qnt[2]

  # Estimate base model: Model 0
  # 1) matrix of all drive variables
  # 2) matrix of all trans_variables
  # 4) travel time to nearest town
  # 5) travel time to nearest rail
  
  m2ols0<-lm(location_value~builtuparea_pct+busyland_pct+restrictedland_pct+localplanrate
             +lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr+lu_greenspace_shr
             +lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass+greenbelt
             +log(drive_BSP)+log(drive_EXE)+log(drive_FAL)+log(drive_PLY)
             +log(trans_BSP)+log(trans_EXE)+log(trans_FAL)+log(trans_PLY)
             +log(distance_coast)+I(log(distance_coast)^2)
             +log(drive_station)+I(log(drive_station)^2),
             data=m2data,subset=iFull,na.action=na.exclude)
  m2ols1<-lm(location_value~builtuparea_pct+busyland_pct+restrictedland_pct+localplanrate
             +lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr+lu_greenspace_shr
             +lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass+greenbelt
             +log(distance_coast)+log(drive_station)
             +spline_drive_BSP+spline_drive_EXE+spline_drive_FAL+spline_drive_PLY
             +spline_trans_BSP+spline_trans_EXE+spline_trans_FAL+spline_trans_PLY,
             data=m2data,subset=iFull,na.action=na.exclude)

  m2ols0$varlist<-B4GetVarList(names(m2ols0$model))
  m2ols1$varlist<-B4GetVarList(names(m2ols1$model))
  
  summary(m2ols0)
  p2ols0<-predict(m2ols0,na.action=na.exclude)
  e20<-location_value[iFull]-p2ols0
  p2ols1<-predict(m2ols1,na.action=na.exclude)
  e21<-location_value[iFull]-p2ols1

  map0<-heatmap1(latitude[iFull],longitude[iFull],e20,filter=!is.na(e20),
                 resolution=0.001,zlabel="residual ols0",outfile=paste0(dirs$outdir,"/errs0.pdf"))
  map1<-heatmap1(latitude[iFull],longitude[iFull],e21,filter=!is.na(e21),
                 resolution=0.001,zlabel="residual ols1",outfile=paste0(dirs$outdir,"/errs1.pdf"))

  plot(m2ols0,pch='.')

  save(m2ols0,file=paste0(dirs$outdir,"/m2ols0.RData"))
  save(m2ols1,file=paste0(dirs$outdir,"/m2ols1.RData"))
}

