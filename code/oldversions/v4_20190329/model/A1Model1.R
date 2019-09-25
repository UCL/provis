# Estimating "Model 1" using B-splines
# log(price_{it})=a_{t}+b'z_{it}+g(e_i,n_i)+u_{it}
# Author: MM, LN
# Last modified: 2018.10.10

# Parameters    
CVFlag<-2       # 0 : No cross-validation on full sample, c.v. on sample1
#                 1 : cross-validation on full sample
#                 2 : construct b-spline basis by hand on full sample, c.v. on sample1
LatLonFlag<-1   # 1 : include interactions between lat and long
N1<-10000        # N1   = size of sample 1
N0<-100000      # N0   = size of fullsample used for estimation
                # N0   = 0 then use all data (excluding outliers)
nregs<-11       # Number of regions
LondonFlag<-0   # 0 : Only Greater London
                # 1 : London + some adjacent counties 
updatedestflag<-1 # 1 : update destinations. 0 : load destinations from file (if file exists)
plotflag<-2     # 0 do not plot
                # 1 plot to screen then save
                # 2 plot directly to file, no screen created
datastub<-"nondom"  # c("m11","nondom")
dataflag<-1
if (datastub=="nondom") {
   dataflag<-2
}

boxcoxflag <- 1 # 0 = estimate (linear,log)
                # 1 = estimate (linear,log,boxcox)
lambda <- matrix(0.1,nregs,2)      # box-cox parameter
                                 # 0 = log
                                 # 1 = linear
                                 # lambda[,1] = m11 parameters
                                 # lambda[,2] = nondom parameters
lambda[,1] <- c(0.089581677,
                0.007956809,
                0.103746691,
                0.059074445,
               -0.019564945,
                0.249716083,
                0.207420182,
                0.029305050,
                -0.022089704,
                0.130751694,
                0.245633442)
lambda[,2] <- c(0.08958168,
                0.03687883,
                0.03994475,
                0.06371014,
                0.03310299,
               -0.02028279,
               -0.029813,
                0.05073838,
                0.02416487,
                0.01016694,
               -0.01781717)

lambda_optimal <- matrix(0,nregs,2)

regnames<-data.frame(region_id=seq(1:nregs), 
                     region_str=c("CaMKOx", "CornwallDevon", 
                                  "EastMid", "EastEng", 
                                  "London", "NE", 
                                  "NW", "SE",
                                  "SW", "WestMid", "YorkshireHumber"))
region_bbx<-vector("list",11)

# Create bounding box for spatial data
wgs.84    <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
ukgrid = "+init=epsg:27700"

uk_bbx <- readWKT("POLYGON((-7.5600 49.9600, 1.7800 49.9600, 1.7800 60.8400, -7.5600 60.8400, -7.5600 49.9600))",
                  p4s=CRS(wgs.84))

for (r in 1:11) {
  region_id<-regnames$region_id[r]    
  region_str<-regnames$region_str[r] 
  dirs<-B2SetPath(RootDir,CodeDir,region_id,datastub)

  # Drop data prior to StartDate
  StartDate<-as.Date("2008-01-01")

  # Load data
  m1data<-read.csv(paste0(dirs$datadir,"/",datastub,"_",region_str,".csv"))

  # Set (long,lat) = (pcd)
  if (datastub=="nondom") {
    m1data$longitude<-m1data$longitude_pcd
    m1data$latitude <-m1data$latitude_pcd
  }
  
  # Add town names
  googlePremium<-TRUE
  if (googlePremium) {
    #devtools::install_github("dkahle/ggmap")
    library(ggmap)
    #ggmap_credentials()
    #register_google(key = "", account_type = "standard", day_limit = 10000)
  }
  m1data<-GetTownNames(m1data)
  
  # Load destinations
  DestinationFile<-paste0(dirs$datadir,"/destinations",region_id,".csv")
  if (!file.exists(DestinationFile) | updatedestflag==1) {
    dest<-B3CreateDestinations(region_id,datastub)
    write.csv(dest,DestinationFile,row.names=FALSE)
  } else {
    dest<-read.csv(DestinationFile)  
  }

  if (datastub=="m11") {
    # M11 data only
    date1<-as.character(m1data$transferdate)
    date2<-as.Date(date1,format="%Y-%m-%d %H:%M")
    i1<-is.na(date2)
    date2[i1]<-as.Date(date1[i1],format="%Y-%m-%d")
    i1<-is.na(date2)
    date2[i1]<-as.Date(date1[i1],format="%d/%m/%Y")

    i1<-grep("transferyear",colnames(m1data))
    colnames(m1data)[i1]<-"transferyear_old"
    m1data$transferyear     <- year(date2)
    m1data$transferdate_str <- date1
    m1data$transferdate     <- date2
    rm(date1,date2,i1)
  
    # Convert prob_4band="" to prob_4band==NA
    i1<-(levels(m1data$prob_4band)=="")
    levels(m1data$prob_4band)[i1]<-NA
  
    if (region_id==3) {
      ikeep <- (m1data$latitude>52)
      m1data<- m1data[ikeep,]
    } else if (region_id==5) {
      ikeep <- (m1data$latitude<51.7)
      m1data<- m1data[ikeep,]
    } else if (region_id==6) {
      ikeep <- (m1data$longitude<= -0.5 & m1data$longitude>= -2.75 &
                m1data$latitude>=54)
      m1data<-m1data[ikeep,]
    } else if (region_id==9) {
      ikeep <- (m1data$longitude>= -4 & m1data$longitude<= -1.2 &
                m1data$latitude<=52.5)
      m1data<-m1data[ikeep,]
    } else if (region_id==10) {
      ikeep <- (m1data$longitude< (-1.1))
      m1data<-m1data[ikeep,]
    }

    # Create time dummies
    m1data$year<-as.factor(m1data$transferyear)
    m1data$month<-as.factor(month(m1data$transferdate))
    m1data<-cbind(m1data,interaction(m1data$year,m1data$month))
    i1<-grep("interaction*",colnames(m1data))
    colnames(m1data)[i1]<-"time_dum"

    iNoMiss<-complete.cases(m1data$pricepaid, m1data$latitude, m1data$longitude, m1data$total_floor_area)
    m1data<-m1data[iNoMiss,]
    m1data$logprice     <- log(m1data$pricepaid)
    m1data$price        <- m1data$pricepaid/1000
  } else {
    
    # drop"Isles of Scilly"
    idrop<- (as.character(m1data$laname)=="Isles of Scilly")
    m1data<-m1data[!idrop,]
    
    # nondom data only
    iNoMiss<-complete.cases(m1data$rateablevalue, m1data$latitude, m1data$longitude, m1data$totalarea)
    m1data<-m1data[iNoMiss,]
    # Drop observatations with rateablevalue==0
    #   These are vacant, under construction or otherwise have zero rateable value
    #   however, economic value is not zero
    izero<-m1data$rateablevalue>0
    m1data<-m1data[izero,]

    m1data$logprice<-log(m1data$rateablevalue)
    m1data$price        <- m1data$rateablevalue/1000

    i1<-grep("totalarea",colnames(m1data))
    colnames(m1data)[i1]<-"total_floor_area"
    
    u1 <- A1NewUseCode(m1data$usecode)
    m1data$usecode <- A1NewUseCode(m1data$usecode)
  }

  if (lambda[region_id,dataflag]==0) {
    m1data$price_boxcox <- m1data$logprice 
  } else {
    m1data$price_boxcox <- (m1data$price^lambda[region_id,dataflag]-1 )/lambda[region_id,dataflag]
  }

  long_lat<-SpatialPoints(m1data[,c("longitude","latitude")],CRS(wgs.84))
  east_north<-spTransform(long_lat,CRS(ukgrid))
  m1data$easting<-coordinates(east_north)[,1]
  m1data$northing<-coordinates(east_north)[,2]
  rm(long_lat,east_north)

  # create interaction (lat_long)
  m1data<-cbind(m1data,m1data$latitude*m1data$longitude)
  i1<-grep("m1data*",colnames(m1data))
  colnames(m1data)[i1]<-"lat_long"

  # Drop outliers in size and price
  qprice <- quantile(m1data$price, probs=c(.001, .999),na.rm=TRUE)
  qsize <-  quantile(m1data$total_floor_area,probs=c(.01,.99),na.rm=TRUE)
  iFull <- m1data$price>qprice[1] & m1data$price<qprice[2] &
           m1data$total_floor_area>qsize[1] & m1data$total_floor_area<qsize[2]
  m1data<-m1data[iFull,]
  
  # create random subsample      
  set.seed(534)
  m1datasample1<-m1data[sample(nrow(m1data), N1), ]
  if (N0>0 & N0<nrow(m1data)) {
    m1data<-m1data[sample(nrow(m1data),N0),]
  }

  # Create bounding box for region
  bbx_str<-paste0("POLYGON((",min(m1data$longitude,na.rm=TRUE)," ",min(m1data$latitude,na.rm=TRUE),",",
                  max(m1data$longitude,na.rm=TRUE)," ",min(m1data$latitude,na.rm=TRUE),",",
                  max(m1data$longitude,na.rm=TRUE)," ",max(m1data$latitude,na.rm=TRUE),",",
                  min(m1data$longitude,na.rm=TRUE)," ",max(m1data$latitude,na.rm=TRUE),",",
                  min(m1data$longitude,na.rm=TRUE)," ",min(m1data$latitude,na.rm=TRUE),"))")
  region_bbx<-readWKT(bbx_str,p4s=CRS(wgs.84),id=as.character(region_str))
  save(region_bbx,file=paste0(dirs$datadir,"/region_bbx.R"))
  
  # create convex hull of region
  houses<-m1datasample1[,c("longitude","latitude")]
  houses<-SpatialPoints(houses,CRS(wgs.84))
  convexhull<-gConvexHull(houses)
  save(convexhull,file=paste0(dirs$datadir,"/convexhull.RData"))
  rm(convexhull,houses)

  # Create some heatmaps

  # Main
  
  # Create formula for model
  vlist0A  <- "logprice~"
  vlist0B  <- "price~"
  vlist0C  <- "price_boxcox~"
  if (datastub=="m11") {
    vlist1         <- "year+propertytype+newbuild+tenure"
  } else if (datastub=="nondom") {
    vlist1 <- "usecode"
  }
  vlist2   <- "+total_floor_area"
  vlist3   <- "+latitude+longitude"
  if (LatLonFlag==1) { 
    vlist3 <- paste0(vlist3,"+lat_long")
  }
  log1    <- as.formula(paste0(vlist0A,vlist1,vlist2,vlist3))
  linear1 <- as.formula(paste0(vlist0B,vlist1,vlist2,vlist3))
  bc1     <- as.formula(paste0(vlist0C,vlist1,vlist2,vlist3))

  # m11 data
  m1log1<-crs(log1,
              data=m1datasample1,basis="additive",kernel=FALSE, cv="nomad",
              degree.max=30, segments.max=50)
  m1linear1<-crs(linear1,
                 data=m1datasample1,basis="additive",kernel=FALSE, cv="nomad",
                 degree.max=30, segments.max=50)
  #    m1boxcox1<-crs(bc1,
  #                     data=m1datasample1,basis="additive",kernel=FALSE, cv="nomad",
  #                     degree.max=30, segments.max=120)
  save(m1log1,file=paste0(dirs$outdir,"/m1log1.RData"))
  save(m1linear1,file=paste0(dirs$outdir,"/m1linear1.RData"))
  #  save(m1boxcox1,file=paste0(dirs$outdir,"/m1boxcox1.RData"))

  m1loghat1<-matrix(predict(m1log1,newdata=m1data))

  if (plotflag>0) {
    tempfile<-paste0(dirs$outdir,"/model1_location0.eps")
    resolution <- 0.1 # you can increase the resolution by decreasing this number
                      # (warning: the resulting dataframe size increase very quickly)
    map0 <- interp(x=m1data$longitude, y=m1data$latitude, z=(m1loghat1-mean(m1loghat1))/sd(m1loghat1), 
                   yo=seq(min(m1data$latitude),max(m1data$latitude),by=resolution), 
                   xo=seq(min(m1data$longitude),max(m1data$longitude),by=resolution), duplicate="mean")
    if (plotflag==2) postscript(file = tempfile)
    # some options for color.palette:
    # terrain.colors
    #rainbow
    # heat.colors
    # topo.colors
    filled.contour(map0, color.palette=rainbow, 
                  plot.title={
                    title(xlab="Longitude",cex.lab=1)
                    mtext("Latitude",2,cex=1,line=3,las=0)
                    mtext("Standardized predicted log price",4,cex=1,line=0.8,las=0)
                  }, plot.axes={points(dest[dest$shortlist,3:4], pch=24);axis(1);axis(2); 
                    text(dest[dest$shortlist,3:4], pos=1, labels=dest[dest$shortlist,2], cex=0.7);
                  }, nlevels=30)
    if (plotflag==1) dev.copy(postscript,tempfile)
    dev.off()

    plotdata<-plot(m1log1,mean=TRUE,ci=TRUE,plot.behavior="data")

    tempfile<- paste0(dirs$outdir,"/model1_size0.eps")
    if (plotflag==2) postscript(tempfile)
    if (datastub=="m11") {
      matplot(plotdata[[5]][,1],plotdata[[5]][,-1],
              xlab="Floor area",ylab="Conditional mean",
              lty=c(1,2,2),col=c(1,2,2),type="l")
    } else {
      matplot(plotdata[[2]][,1],plotdata[[2]][,-1],
              xlab="Floor area",ylab="Conditional mean",
              lty=c(1,2,2),col=c(1,2,2),type="l")
    }
    if (plotflag==1) dev.copy(postscript,tempfile)
    dev.off()

    tempfile<- paste0(dirs$outdir,"/model1_price_sqm_1.eps")
    if (plotflag==2) postscript(tempfile)
    if (datastub=="m11") {
      matplot(plotdata[[5]][,1],exp(plotdata[[5]][,-1])/plotdata[[5]][,1],
              xlab="Floor area",ylab="Price per sqm",
              lty=c(1,2,2),col=c(1,2,2),type="l")
    } else {
      matplot(plotdata[[2]][,1],exp(plotdata[[2]][,-1])/plotdata[[2]][,1],
              xlab="Floor area",ylab="Price per sqm ",
              xlim=c(0,300),
              lty=c(1,2,2),col=c(1,2,2),type="l")
    }  
    if (plotflag==1) dev.copy(postscript,tempfile)
    dev.off()
    
    tempfile<- paste0(dirs$outdir,"/model1_price_sqm_2.eps")
    if (plotflag==2) postscript(tempfile)
    if (datastub=="m11") {
    } else {
      matplot(plotdata[[2]][,1],exp(plotdata[[2]][,-1])/plotdata[[2]][,1],
              xlab="Floor area",ylab="Price per sqm ",
              xlim=c(300,max(plotdata[[2]][,1])),
              lty=c(1,2,2),col=c(1,2,2),type="l")
    }  
    if (plotflag==1) dev.copy(postscript,tempfile)
    dev.off()
    
    tempfile<-paste0(dirs$outdir,'/model1_lat0.eps')
    if (plotflag==2) postscript(tempfile)
    if (datastub=="m11") {
      matplot(plotdata[[6]][,1],plotdata[[6]][,-1],
              xlab="Latitude",ylab="Conditional mean",
              lty=c(1,2,2),col=c(1,2,2),type="l")
    } else {
      matplot(plotdata[[3]][,1],plotdata[[3]][,-1],
              xlab="Latitude",ylab="Conditional mean",
              lty=c(1,2,2),col=c(1,2,2),type="l")
    }
    if (plotflag==1)dev.copy(postscript,tempfile)
    dev.off()

    tempfile<-paste0(dirs$outdir,'/model1_lon0.eps')
    if (plotflag==2) postscript(tempfile)
    if (datastub=="m11") {
      matplot(plotdata[[7]][,1],plotdata[[7]][,-1],
              xlab="Longitude",ylab="Conditional mean",
              lty=c(1,2,2),col=c(1,2,2),type="l")
    } else {
      matplot(plotdata[[4]][,1],plotdata[[4]][,-1],
              xlab="Longitude",ylab="Conditional mean",
              lty=c(1,2,2),col=c(1,2,2),type="l")
    }  
    if (plotflag==1) dev.copy(postscript,tempfile)
    dev.off()
  }

  if (CVFlag==0) {
    # Skip cross-validation for full sample
    # use results (degree,segment,lambda,include) from m1log1
    m1log0<-crs(log1,
                data=m1data,basis="additive", kernel=FALSE,cv="none",
                degree=m1log1$degree,segments=m1log1$segments,
               lambda=m1log1$lambda,include=m1log1$include)
    m1linear0<-crs(linear1,
                   data=m1data,basis="additive", kernel=FALSE,cv="none",
                   degree=m1log1$degree,segments=m1log1$segments,
                   lambda=m1log1$lambda,include=m1log1$include)
#    m1boxcox0<-crs(bc1,
#                     data=m1data,basis="additive", kernel=FALSE,cv="none",
#                     degree=m1log1$degree,segments=m1log1$segments,
#                     lambda=m1log1$lambda,include=m1log1$include)
  } else if (CVFlag==1) { 
    # Use Cross-validation for full sample
    m1log0<-crs(log1,
                data=m1data,basis="additive", kernel=FALSE, cv="nomad",
                degree.max=10, segments.max=50)
    m1linear0<-crs(linear1,
                   data=m1data,basis="additive", kernel=FALSE, cv="nomad",
                   degree.max=10, segments.max=50)
 #   m1boxcox0<-crs(bc1,
#                   data=m1data,basis="additive", kernel=FALSE, cv="nomad",
#                   degree.max=10, segments.max=50)
  } else if (CVFlag==2) {
    # Set up splines
    degree   <- m1log1$degree
    segments <- m1log1$segments
    if (LatLonFlag==0) {
      knots<-list(quantile(m1data$total_floor_area,probs=seq(0.1,0.9,length.out=segments[1])), 
                  quantile(m1data$latitude,probs=seq(0.1,0.9,length.out=segments[2])),
                  quantile(m1data$longitude,probs=seq(0.1,0.9,length.out=segments[3])))
    } else {
      knots<-list(quantile(m1data$total_floor_area,probs=seq(0.1,0.9,length.out=segments[1])), 
                  quantile(m1data$latitude,probs=seq(0.1,0.9,length.out=segments[2])),
                  quantile(m1data$longitude,probs=seq(0.1,0.9,length.out=segments[3])),
                  quantile(m1data$lat_long,probs=seq(0.1,0.9,length.out=segments[4])))
    }
  
    m1data$bSize<-bSpline(m1data$total_floor_area,knots=knots[[1]],degree=degree[1],intercept=FALSE)
    m1data$bLat<-bSpline(m1data$latitude,knots=knots[[2]],degree=degree[2],intercept=FALSE)
    m1data$bLon<-bSpline(m1data$longitude,knots=knots[[3]],degree=degree[3],intercept=FALSE)
    if (LatLonFlag==1) {
      m1data$bLatLon<-bSpline(m1data$lat_long,knots=knots[[4]],degree=degree[4],intercept=FALSE)
    }

    # Estimate model
    # Formulas
    vlist4  <- "+bSize+bLat+bLon"
    if (LatLonFlag==1) {
      vlist4<-paste0(vlist4,"+bLatLon")  
    }
    log2    <- as.formula(paste0(vlist0A,vlist1,vlist4))
    linear2 <- as.formula(paste0(vlist0B,vlist1,vlist4))
    bc2     <- as.formula(paste0(vlist0C,vlist1,vlist4))
 
    m1log0<-lm(log2,data=m1data)
    m1linear0<-lm(linear2,data=m1data)
    m1boxcox0<-lm(bc2,data=m1data)
    if (boxcoxflag==1) {
      bc0 <- boxcox(m1linear0,optimize=TRUE)
      lambda_optimal[region_id,dataflag] <- bc0$lambda
    }
    # Save spline details
    m1log0$knots<-knots
    m1log0$degree<-degree
    m1log0$segments<-segments
    m1log0$LatLonFlag<-LatLonFlag
  
    m1linear0$knots<-knots
    m1linear0$degree<-degree
    m1linear0$segments<-segments
    m1linear0$LatLonFlag<-LatLonFlag
  
    m1boxcox0$knots<-knots
    m1boxcox0$degree<-degree
    m1boxcox0$segments<-segments
    m1boxcox0$LatLonFlag<-LatLonFlag
    m1boxcox0$lambda<-lambda[region_id,dataflag]  # box cox parameter
  } # else if (CVFLAG==2) {

  m1log0$CVFlag    <- CVFlag
  m1linear0$CVFlag <- CVFlag
  m1boxcox0$CVFlag <- CVFlag
  save(m1log0,file=paste0(dirs$outdir,"/m1log0.RData"))
  save(m1linear0,file=paste0(dirs$outdir,"/m1linear0.RData"))
  save(m1boxcox0,file=paste0(dirs$outdir,"/m1boxcox0.RData"))
  attach(m1data)

  if (CVFlag<2) {
    plotdata_full<-plot(m1log0,mean=TRUE,ci=TRUE,plot.behavior="data")
    if (plotflag>0) {
      tempfile<-paste0(dirs$outdir,'/model1_size0.eps')
      if (plotflag==2) postscript(tempfile)
      matplot(plotdata_full[[5]][,1],plotdata_full[[5]][,-1],
              xlab="Floor area",ylab="Conditional mean",
              lty=c(1,2,2),col=c(1,2,2),type="l")
      if (plotflag==1) dev.copy(postscript,tempfile)
      dev.off()
  
      tempfile<-paste0(dirs$outdir,'/model1_lat0.eps')
      if (plotflag==2) postscript(tempfile)
      matplot(plotdata_full[[6]][,1],plotdata_full[[6]][,-1],
              xlab="Latitude",ylab="Conditional mean",
              lty=c(1,2,2),col=c(1,2,2),type="l")
      if (plotflag==1) dev.copy(postscript,tempfile)  
      dev.off()

      tempfile<-paste0(dirs$outdir,'/model1_lon0.eps')
      if (plotflag==2) postscript(tempfile)
      matplot(plotdata_full[[7]][,1],plotdata_full[[7]][,-1],
              xlab="Longitude",ylab="Conditional mean",
              lty=c(1,2,2),col=c(1,2,2),type="l")
      if (plotflag==1) dev.copy(postscript,tempfile)
      dev.off()
    }
    # Recovering g(e,n)
    m1splinecoef<-matrix(m1log0$model.lm$coefficients)
    m1splinem<-m1log0$model.lm$model
    m1splinex<-m1splinem$P
    m1splinex<-cbind(rep(1,dim(m1splinex)[1]),m1splinex)
    hatvals<-m1splinex %*% m1splinecoef # check if you get the same fitted values
    hatvals==m1log0$fitted.values

    sp_deg_area<-m1spline_fullsample$degree[1]
    sp_seg_area<-m1spline_fullsample$segments[1]
    sp_deg_lat<-m1spline_fullsample$degree[2]
    sp_seg_lat<-m1spline_fullsample$segments[2]
    sp_deg_lon<-m1spline_fullsample$degree[3]
    sp_seg_lon<-m1spline_fullsample$segments[3]

    location_value<-m1splinex[,(sp_deg_area+sp_seg_area+1):(sp_deg_area+sp_seg_area+sp_deg_lat+sp_seg_lat+sp_deg_lon+sp_seg_lon-2)] %*% 
     m1splinecoef[(sp_deg_area+sp_seg_area+1):(sp_deg_area+sp_seg_area+sp_deg_lat+sp_seg_lat+sp_deg_lon+sp_seg_lon-2)]
    warning(paste0("CVFlag < 2. This code segment only computes results for log prices. ",
                   "It does not compute results for the outcomes 'price' and 'price_boxcox'."))
  } else if (CVFlag==2) {
    # Predict location value from bSpline model
    logvalue    <-  A1PredictValue(m1log0,m1data,LatLonFlag)
    value       <-  A1PredictValue(m1linear0,m1data,LatLonFlag)
    boxcoxvalue <-  A1PredictValue(m1boxcox0,m1data,LatLonFlag)
  }
    
  if (plotflag>0) {
    res_long <- (max(m1data$longitude)-min(m1data$longitude))/40
    res_lat  <- (max(m1data$latitude)-min(m1data$latitude))/40
    
    A1Plot(logvalue$location_value,
           logvalue$structure_value,m1data,dest,vname="logvalue",
           lambda[region_id,dataflag],dirs,plotflag,resolution=min(res_long,res_lat)) 
    A1Plot(value$location_value,
           value$structure_value,m1data,dest,vname="value",
           lambda[region_id,dataflag],dirs,plotflag,resolution=min(res_long,res_lat)) 
    A1Plot(boxcoxvalue$location_value,
           boxcoxvalue$structure_value,m1data,dest,vname="boxcoxvalue",
           lambda[region_id,dataflag],dirs,plotflag,resolution=min(res_long,res_lat)) 
  }
 
  # Template for m1data for counterfactual predictions
  m1data_template<-m1data[1,]
  save(m1data_template,file=paste0(dirs$newdatadir,"/m1data_template.RData"))
  m2data<- data.frame(logvalue$location_value,
                      value$location_value,
                      boxcoxvalue$location_value,m1data)
  names(m2data)[1:3]<-c("logvalue","value","boxcoxvalue")
  save(m2data,file=paste0(dirs$datadir,"/m2data1.RData"))
  save(m1data,file=paste0(dirs$datadir,"/m1data.RData"))
} # if (region in 1:11) 
