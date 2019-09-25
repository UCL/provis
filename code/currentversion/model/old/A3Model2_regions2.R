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
  
  # Get names of drive/trans
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
  
  m2ols0<-lm(as.formula(paste0("location_value~builtuparea_pct+busyland_pct+restrictedland_pct+localplanrate+lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr+lu_greenspace_shr+lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass+greenbelt+log(distance_coast)+I(log(distance_coast)^2)+log(drive_station)+I(log(drive_station)^2)",pf)),
             data=m2data,subset=iFull,na.action=na.exclude)

  m2ols1<-lm(as.formula(paste0("location_value~builtuparea_pct+busyland_pct+localplanrate+lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr+lu_greenspace_shr+lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass+greenbelt+log(distance_coast)+I(log(distance_coast)^2)+log(drive_station)+I(log(drive_station)^2)",sf)),
             data=m2data,na.action=na.exclude)
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

  # lassovars<-m2ols0$varlist
  # lassodata<-complete.cases(m2data[lassovars])
  # lassodata<-m2data[lassodata,]
  # 
  # mod0<-model.matrix(location_value~builtuparea_pct+busyland_pct+restrictedland_pct+localplanrate
  #          +lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr+lu_greenspace_shr
  #          +lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass+greenbelt
  #          +log(drive_BSP)+log(drive_EXE)+log(drive_FAL)+log(drive_PLY)
  #          +log(trans_BSP)+log(trans_EXE)+log(trans_FAL)+log(trans_PLY)
  #          +log(distance_coast)+I(log(distance_coast)^2)
  #          +log(drive_station)+I(log(drive_station)^2),
  #          data=lassodata)[,-1]
  # 
  # attach(lassodata)
  # 
  # lambda <- 10^seq(10, -2, length = 100)
  # 
  # set.seed(489)
  # train = sample(1:nrow(mod0), nrow(mod0)/2)
  # test = (-train)
  # ytest = location_value[test]
  # 
  # cv.out <- cv.glmnet(mod0[train,], location_value[train], alpha = 0)
  # bestlam <- cv.out$lambda.min
  # 
  # m2ols0_tr<- lm(location_value~builtuparea_pct+busyland_pct+restrictedland_pct+localplanrate
  #                +lu_domestic_shr+lu_gardens_shr+lu_nondom_shr+lu_road_shr+lu_rail_shr+lu_greenspace_shr
  #                +lu_water_shr+popdensityOA+imddecile+prob_4band+noiseclass+greenbelt
  #                +log(drive_BSP)+log(drive_EXE)+log(drive_FAL)+log(drive_PLY)
  #                +log(trans_BSP)+log(trans_EXE)+log(trans_FAL)+log(trans_PLY)
  #                +log(distance_coast)+I(log(distance_coast)^2)
  #                +log(drive_station)+I(log(drive_station)^2), data = lassodata, subset = train)
  # m2ridge_tr <- glmnet(mod0[train,], location_value[train], alpha = 0, lambda = lambda)
  # 
  # #make predictions
  # m2ridge_pred <- predict(m2ridge_tr, s = bestlam, newx = mod0[test,])
  # m2ols_pred <- predict(m2ols0_tr, newdata = lassodata[test,])
  # 
  # #check MSE
  # mean((m2ridge_pred-ytest)^2)
  # mean((m2ols_pred-ytest)^2)
  # 
  # cv.out <- cv.glmnet(mod0[train,], location_value[train], alpha = 1)
  # bestlam <- cv.out$lambda.min
  # 
  # lasso.mod <- glmnet(mod0[train,], location_value[train], alpha = 1, lambda = bestlam)
  # m2lasso_pred<-predict(lasso.mod, s = bestlam, newx = mod0[test,])
  # mean((m2lasso_pred-ytest)^2)
  
  
  summary(m2ols0)
  
  p2ols0<-predict(m2ols0,na.action=na.exclude)
  e20<-location_value[iFull]-p2ols0
  p2ols1<-predict(m2ols1,na.action=na.exclude)
  e21<-location_value[iFull]-p2ols1[iFull]
  p2ols2<-predict(m2ols2,na.action=na.exclude)
  e22<-location_value[iFull]-p2ols2
  

  map0<-heatmap1(latitude[iFull],longitude[iFull],e20,filter=!is.na(e20),
                 resolution=0.001,zlabel="residual ols0",outfile=paste0(dirs$outdir,"/errs0.pdf"))
  map1<-heatmap1(latitude[iFull],longitude[iFull],e21,filter=!is.na(e21),
                 resolution=0.001,zlabel="residual ols1",outfile=paste0(dirs$outdir,"/errs1.pdf"))
  map2<-heatmap1(latitude[iFull],longitude[iFull],e22,filter=!is.na(e22),
                 resolution=0.001,zlabel="residual ols2",outfile=paste0(dirs$outdir,"/errs2.pdf"))
  
  plot(m2ols1,pch='.')

  save(m2ols0,file=paste0(dirs$outdir,"/m2ols0.RData"))
  save(m2ols1,file=paste0(dirs$outdir,"/m2ols1.RData"))
  save(m2ols2,file=paste0(dirs$outdir,"/m2ols2.RData"))
  
}

