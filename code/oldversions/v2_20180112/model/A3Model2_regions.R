# Estimate model 2
# Author:  MM,LN
# Version: 2018.01.01

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
plotflag<- 0  # 0 = no plots.  1 = plot some results

for (r in c(1:4,6:11)) {
  region_id<-regnames$region_id[r]    
  region_str<-as.character(regnames$region_str[r]) 
  dirs<-B2SetPath(RootDir,CodeDir,region_id)
  # Load data
  load(file=paste0(dirs$datadir,"/m2data2.RData"))

  # Define subsamples
  qnt <- quantile(m2data$location_value, probs=c(.01, .99),na.rm=TRUE)
  iFull <- (m2data$location_value>qnt[1] & m2data$location_value<qnt[2])

  # Estimate base model: Model 0
  # 1) matrix of all drive variables
  # 2) matrix of all trans_variables
  # 4) travel time to nearest town
  # 5) travel time to nearest rail
  
  # List of all variables starting with "drive_"
  # Create minimum drive time
  drive_vars<-ls(m2data, pattern="^drive")
  istn<-which(drive_vars=="drive_station")
  drive_vars<-drive_vars[-istn]
  f2<-function(x) min(m2data[x,drive_vars],na.rm=TRUE)
  m2data$mindrive<-tapply(1:nrow(m2data),1:nrow(m2data),f2)
  m2data$mindrive[m2data$mindrive==Inf]<-NA

  # List all variables with "trans_"
  # Create minimum transit time 
  transit_vars<-ls(m2data, pattern="^trans_")
  f3<-function(x) min(m2data[x,transit_vars],na.rm=TRUE)
  m2data$mintrans<-tapply(1:nrow(m2data),1:nrow(m2data),f3)
  m2data$mintrans[m2data$mintrans==Inf]<-NA
  
  # Get names of drive/trans and creates character strings for formula
  commute_vars<-c(drive_vars,transit_vars)
  commute_splines<-c(ls(m2data,pattern="spline_drive"), ls(m2data,pattern="spline_trans"))
  istn<-which(commute_splines=="spline_drive_station")
  commute_splines<-commute_splines[-istn]
  lf<-function(x) paste0("log(",x,")")
  lcvrs<-lf(commute_vars)
  pf<-""
  for (i in 1:length(lcvrs)) {pf<-paste(pf,lcvrs[i], sep="+") }
  sf<-""
  for (i in 1:length(commute_splines)) {sf<-paste(sf,commute_splines[i], sep="+") }
  
  formula0<-as.formula(paste0("location_value~builtuparea_pct+restrictedland_pct+localplanrate+",
                              "lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr+",
                              "lu_greenspace_shr+lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass+",
                              "greenbelt+","
                              log(drive_station)+I(log(drive_station)^2)",pf))
  formula1<-as.formula(paste0("location_value~builtuparea_pct+restrictedland_pct+localplanrate+",
                              "lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr+",
                              "lu_greenspace_shr+lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass+",
                              "greenbelt+","log(drive_station)+I(log(drive_station)^2)",sf))
  if (region_id==5) {
     # coast  is irrelevant for london
     formula0<-as.formula(paste0("location_value~builtuparea_pct+busyland_pct+restrictedland_pct+localplanrate+",
                              "lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr+",
                              "lu_greenspace_shr+lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass+",
                              "greenbelt+","
                              log(drive_station)+I(log(drive_station)^2)",pf))
  formula1<-as.formula(paste0("location_value~builtuparea_pct+busyland_pct+restrictedland_pct+localplanrate+",
                              "lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr+",
                              "lu_greenspace_shr+lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass+",
                              "greenbelt+","
                              log(drive_station)+I(log(drive_station)^2)",sf))
  } 
  m2ols0<-lm(formula0,data=m2data,subset=iFull,na.action=na.exclude)

  m2ols1<-lm(formula1,data=m2data,na.action=na.exclude)
  m2ols2<-lm(location_value~builtuparea_pct+busyland_pct+restrictedland_pct+localplanrate
                +lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr+lu_greenspace_shr
                +lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass+greenbelt
                +log(mindrive)+I(log(mindrive)^2)
                +log(mintrans)+I(log(mintrans)^2)
                +log(distance_coast)+I(log(distance_coast)^2)
                +log(drive_station)+I(log(drive_station)^2),
                data=m2data,subset=iFull,na.action=na.exclude)

  m2ols0$varlist<-B4GetVarList(names(m2ols0$model))
  m2ols1$varlist<-B4GetVarList(names(m2ols1$model))
  
  summary(m2ols0)
  
  p2ols0<-predict(m2ols0,na.action=na.exclude)
  e20<-m2data$location_value[iFull]-p2ols0
  p2ols1<-predict(m2ols1,na.action=na.exclude)
  e21<-m2data$location_value[iFull]-p2ols1[iFull]
  p2ols2<-predict(m2ols2,na.action=na.exclude)
  e22<-m2data$location_value[iFull]-p2ols2

  if (plotflag==1) {   
    map0<-heatmap1(m2data$latitude[iFull],m2data$longitude[iFull],e20,filter=!is.na(e20),
                   resolution=0.001,zlabel="residual ols0",outfile=paste0(dirs$outdir,"/errs0.pdf"))
    map1<-heatmap1(m2data$latitude[iFull],m2data$longitude[iFull],e21,filter=!is.na(e21),
                   resolution=0.001,zlabel="residual ols1",outfile=paste0(dirs$outdir,"/errs1.pdf"))
    map2<-heatmap1(m2data$latitude[iFull],m2data$longitude[iFull],e22,filter=!is.na(e22),
                   resolution=0.001,zlabel="residual ols2",outfile=paste0(dirs$outdir,"/errs2.pdf"))
  
    plot(m2ols1,pch='.')
  }
  save(m2ols0,file=paste0(dirs$outdir,"/m2ols0.RData"))
  save(m2ols1,file=paste0(dirs$outdir,"/m2ols1.RData"))
  save(m2ols2,file=paste0(dirs$outdir,"/m2ols2.RData"))
  
}

