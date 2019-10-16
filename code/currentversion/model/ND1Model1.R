# Estimating "Model 1" using B-splines
# log(price_{it})=a_{t}+b'z_{it}+g(e_i,n_i)+u_{it}
# Author: MM, LN
# Last modified: 2018/02/02

# Parameters    
CVFlag<-2       # 0 : No cross-validation on full sample, c.v. on sample1
#                 1 : cross-validation on full sample
#                 2 : construct b-spline basis by hand on full sample, c.v. on sample1
LatLonFlag<-1   # 1 : include interactions between lat and long
N1<-5000        # N1   = size of sample 1
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
boxcoxflag <- 1 # 0 = estimate (linear,log)
                # 1 = estimate (linear,log,boxcox)
lambda <- matrix(-0.22,nregs)  # box-cox parameter
                               # 0 = log
                               # 1 = linear
lambda <- c(0.089581677,
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

lambda_optimal <- matrix(0,nregs)

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

for (r in 3:3) {

  region_id<-regnames$region_id[r]    
  region_str<-regnames$region_str[r] 
  dirs<-B2SetPath(RootDir,CodeDir,DataRoot,region_id,datastub)

  # Drop data prior to StartDate
  StartDate<-as.Date("2008-01-01")

  # Load data
  m1data<-read.csv(paste0(dirs$datadir,"/",datastub,"_",region_str,".csv"))

  # Set (long,lat) = (pcd)
  m1data$longitude<-m1data$longitude_pcd
  m1data$latitude <-m1data$latitude_pcd

  # Add town names
  googlePremium<-TRUE
  if (googlePremium) {
    devtools::install_github("dkahle/ggmap")
    library(ggmap)
    #ggmap_credentials()
    #register_google(key = "SECRET KEy", account_type = "premium", day_limit = 100000)
  }
  m1data<-GetTownNames(m1data,datastub)
  
  # Load destinations
  DestinationFile<-paste0(dirs$datadir,"/destinations",region_id,".csv")
  if (!file.exists(DestinationFile) | updatedestflag==1) {
    dest<-B3CreateDestinations(region_id)
    write.csv(dest,DestinationFile,row.names=FALSE)
  } else {
    dest<-read.csv(DestinationFile)  
  }

  if (data_stub=="m11") {
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
    # NON-DOM data only
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
  }

  if (lambda[region_id]==0) {
    m1data$price_boxcox <- m1data$logprice 
  } else {
    m1data$price_boxcox <- (m1data$price^lambda[region_id]-1 )/lambda[region_id]
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
  if (N0>0) {
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
  # m11 data
  if (data_stub=="m11") {
    if (LatLonFlag==0) {
      m1log1<-crs(logprice~year+propertytype+newbuild+tenure+total_floor_area
                  +latitude+longitude,
                  data=m1datasample1,basis="additive",kernel=FALSE, cv="nomad",
                  degree.max=30, segments.max=120)
      m1linear1<-crs(price~year+propertytype+newbuild+tenure+total_floor_area
                     +latitude+longitude,
                     data=m1datasample1,basis="additive",kernel=FALSE, cv="nomad",
                     degree.max=30, segments.max=120)
  #    m1boxcox1<-crs(price_boxcox~year+propertytype+newbuild+tenure+total_floor_area
  #                     +latitude+longitude,
  #                     data=m1datasample1,basis="additive",kernel=FALSE, cv="nomad",
  #                     degree.max=30, segments.max=120)
    } else if (LatLonFlag==1) {
      m1log1<-crs(logprice~year+propertytype+newbuild+tenure+total_floor_area
                   +latitude+longitude+lat_long,
                   data=m1datasample1,basis="additive",kernel=FALSE, cv="nomad",
                   degree.max=50, segments.max=100)
      m1linear1<-crs(price~year+propertytype+newbuild+tenure+total_floor_area
                     +latitude+longitude+lat_long,
                     data=m1datasample1,basis="additive",kernel=FALSE, cv="nomad",
                     degree.max=50, segments.max=100)
  #    m1boxcox1<-crs(price_boxcox~year+propertytype+newbuild+tenure+total_floor_area
  #                    +latitude+longitude+lat_long,
  #                   data=m1datasample1,basis="additive",kernel=FALSE, cv="nomad",
  #                   degree.max=50, segments.max=100)
    }
  } else {
    # nondom model
    if (LatLonFlag==0) {
      m1log1<-crs(logprice~usecode+totalarea
                  +latitude+longitude,
                  data=m1datasample1,basis="additive",kernel=FALSE, cv="nomad",
                  degree.max=30, segments.max=120)
      m1linear1<-crs(price~usecode+totalarea
                  +latitude+longitude,
                  data=m1datasample1,basis="additive",kernel=FALSE, cv="nomad",
                  degree.max=30, segments.max=120)
    } else if (LatLonFlag==1) {
      m1log1<-crs(logprice~usecode+totalarea
                     +latitude+longitude+lat_long,
                     data=m1datasample1,basis="additive",kernel=FALSE, cv="nomad",
                     degree.max=50, segments.max=200)
      m1linear1<-crs(price~usecode+totalarea
                     +latitude+longitude+lat_long,
                     data=m1datasample1,basis="additive",kernel=FALSE, cv="nomad",
                     degree.max=50, segments.max=200)

    }
  }
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
    matplot(plotdata[[5]][,1],plotdata[[5]][,-1],
            xlab="Floor area",ylab="Conditional mean",
            lty=c(1,2,2),col=c(1,2,2),type="l")
    if (plotflag==1) dev.copy(postscript,tempfile)
    dev.off()

    tempfile<-paste0(dirs$outdir,'/model1_lat0.eps')
    if (plotflag==2) postscript(tempfile)
    matplot(plotdata[[6]][,1],plotdata[[6]][,-1],
            xlab="Latitude",ylab="Conditional mean",
            lty=c(1,2,2),col=c(1,2,2),type="l")
    if (plotflag==1)dev.copy(postscript,tempfile)
    dev.off()

    tempfile<-paste0(dirs$outdir,'/model1_lon0.eps')
    if (plotflag==2) postscript(tempfile)
    matplot(plotdata[[7]][,1],plotdata[[7]][,-1],
            xlab="Longitude",ylab="Conditional mean",
            lty=c(1,2,2),col=c(1,2,2),type="l")
    if (plotflag==1) dev.copy(postscript,tempfile)
    dev.off()
  }

  if (CVFlag==0) {
    # Skip cross-validation for full sample
    # use results (degree,segment,lambda,include) from m1log1
    m1spline0<-crs(logprice~usecode+totalarea
                   +latitude+longitude+lat_long,
                   data=m1data,basis="additive", kernel=FALSE,cv="none",
                   degree=m1spline1$degree,segments=m1spline1$segments,
                   lambda=m1spline1$lambda,include=m1spline1$include)
  } else if (CVFlag==1) { 
    # Use Cross-validation for full sample
    m1spline0<-crs(logprice~usecode+totalarea
                            +latitude+longitude,
                             data=m1data,basis="additive", kernel=FALSE, cv="nomad",
                             degree.max=10, segments.max=50)
  } else if (CVFlag==2) {
    # Set up splines
    degree<-m1spline1$degree
    segments<-m1spline1$segments
    if (LatLonFlag==0) {
      knots<-list( quantile(m1data$totalarea,probs=seq(0.1,0.9,length.out=segments[1])), 
                   quantile(m1data$latitude,probs=seq(0.1,0.9,length.out=segments[2])),
                   quantile(m1data$longitude,probs=seq(0.1,0.9,length.out=segments[3])))
    } else {
      knots<-list( quantile(m1data$totalarea,probs=seq(0.1,0.9,length.out=segments[1])), 
                   quantile(m1data$latitude,probs=seq(0.1,0.9,length.out=segments[2])),
                   quantile(m1data$longitude,probs=seq(0.1,0.9,length.out=segments[3])),
                   quantile(m1data$lat_long,probs=seq(0.1,0.9,length.out=segments[4])))
    }
  
  m1data$bSize<-bSpline(m1data$totalarea,knots=knots[[1]],degree=degree[1],intercept=FALSE)
  m1data$bLat<-bSpline(m1data$latitude,knots=knots[[2]],degree=degree[2],intercept=FALSE)
  m1data$bLon<-bSpline(m1data$longitude,knots=knots[[3]],degree=degree[3],intercept=FALSE)
  if (LatLonFlag==1) {
    m1data$bLatLon<-bSpline(m1data$lat_long,knots=knots[[4]],degree=degree[4],intercept=FALSE)
    
  }

  # Estimate model  
  if (LatLonFlag==0) {
    m1spline0<-lm(logprice~usecode
                            +bSize+bLat+bLon,
                            data=m1data)
  } else {
    m1spline0<-lm(logprice~usecode
                  +bSize+bLat+bLon+bLatLon,
                  data=m1data)
  }
  # Save spline details
  m1spline0$knots<-knots
  m1spline0$degree<-degree
  m1spline0$segments<-segments
  m1spline0$LatLonFlag<-LatLonFlag
  
}

m1spline0$CVFlag<-CVFlag
save(m1spline0,file=paste0(dirs$outdir,"/m1spline0.RData"))

if (CVFlag<2) {
  plotdata_full<-plot(m1spline0,mean=TRUE,ci=TRUE,plot.behavior="data")

  if (plotflag>0) {
    if (plotflag==2) {
      pdf(paste0(dirs$outdir,"/area_model1_full.pdf"))
    }
    matplot(plotdata_full[[2]][,1],plotdata_full[[2]][,-1],
          xlab="Floor area",ylab="Conditional mean",
          lty=c(1,2,2),col=c(1,2,2),type="l")
    if (plotflag==1) {
      dev.copy(pdf,paste0(dirs$outdir,"/area_model1_full.pdf"))
    }
    dev.off()
  
    if (plotflag==2) {
      pdf(paste0(dirs$outdir,"/lat_model1_full.pdf"))
    }
    matplot(plotdata_full[[3]][,1],plotdata_full[[3]][,-1],
          xlab="Latitude",ylab="Conditional mean",
          lty=c(1,2,2),col=c(1,2,2),type="l")
    if (plotflag==1) {
      dev.copy(pdf,paste0(dirs$outdir,"/lat_model1_full.pdf"))
    }
    dev.off()

    if (plotflag==2) {
      pdf(paste0(dirs$outdir,"/lon_model1_full.pdf"))
    }
    matplot(plotdata_full[[4]][,1],plotdata_full[[4]][,-1],
          xlab="Longitude",ylab="Conditional mean",
          lty=c(1,2,2),col=c(1,2,2),type="l")
    if (plotflag==1) {
      dev.copy(pdf,paste0(dirs$outdir,"/lon_model1_full.pdf"))
    }
    dev.off()
  }
  # Recovering g(e,n)
  m1splinecoef<-matrix(m1spline0$model.lm$coefficients)
  m1splinem<-m1spline0$model.lm$model
  m1splinex<-m1splinem$P
  m1splinex<-cbind(rep(1,dim(m1splinex)[1]),m1splinex)
  hatvals<-m1splinex %*% m1splinecoef # check if you get the same fitted values
  hatvals==m1spline0$fitted.values

  sp_deg_area<-m1spline_fullsample$degree[1]
  sp_seg_area<-m1spline_fullsample$segments[1]
  sp_deg_lat<-m1spline_fullsample$degree[2]
  sp_seg_lat<-m1spline_fullsample$segments[2]
  sp_deg_lon<-m1spline_fullsample$degree[3]
  sp_seg_lon<-m1spline_fullsample$segments[3]

  location_value<-m1splinex[,(sp_deg_area+sp_seg_area+1):(sp_deg_area+sp_seg_area+sp_deg_lat+sp_seg_lat+sp_deg_lon+sp_seg_lon-2)] %*% 
   m1splinecoef[(sp_deg_area+sp_seg_area+1):(sp_deg_area+sp_seg_area+sp_deg_lat+sp_seg_lat+sp_deg_lon+sp_seg_lon-2)]
} else if (CVFlag==2) {
  # Predict location value from bSpline model
  
  # Find indexes for spline coefficients
  isize<-grep("bSize",names(m1spline0$coefficients))
  ilat<-grep("bLat[123456789]",names(m1spline0$coefficients))
  ilon<-grep("bLon",names(m1spline0$coefficients))
  ilatlon<-grep("bLatLon",names(m1spline0$coefficients))
  # Predict:
  #  1) logprice0 = predicted log price
  #  2) logprice1 = component of log price that varies with total_floor_area
  #  3) logprice2 = component of log price that varies with latitude
  #  4) logprice3 = component of log price that varies with longitude
  logprice0<-predict(m1spline0,newdata=m1data)
  logprice1<-m1data$bSize %*% 
    m1spline0$coefficients[isize]
  logprice2<-m1data$bLat %*%
    m1spline0$coefficients[ilat]
  logprice3<-m1data$bLon %*% 
    m1spline0$coefficients[ilon]
  if (LatLonFlag==1) {
    logprice4<-m1data$bLatLon %*% m1spline0$coefficients[ilatlon]  
  }
  
  # Recovering g(e,n)
  location_value<-logprice2+logprice3
  if (LatLonFlag==1) {
    location_value <- location_value+logprice4
  }
}
    
if (plotflag>0) {
  if (plotflag==2) {
    pdf(paste0(dirs$outdir,"/density_g_en_full.pdf"))
  }
   plot(density(location_value), main=expression(g(e[i],n[i])))
  if (plotflag==1) {
    dev.copy(pdf,paste0(dirs$outdir,"/density_g_en_full.pdf"))
  }
  dev.off()

  std_lv<-(location_value-mean(location_value))/sd(location_value)
 
  resolution <- 0.001 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)

  map5f <- interp(x=longitude, y=latitude, z=std_lv, 
                 yo=seq(min(latitude),max(latitude),by=resolution), 
                 xo=seq(min(longitude),max(longitude),by=resolution), duplicate="mean")
  if (plotflag==2) {
    postscript(file=paste0(dirs$outdir,"/map5f_model1.eps"))
  }
  filled.contour(map5f, color.palette=heat.colors, 
                 plot.title={
                  title(xlab="Longitude",cex.lab=1)
                  mtext("Latitude",2,cex=1,line=3,las=0)
                  mtext("Standardized location value",4,cex=1,line=0.8,las=0)
                }, plot.axes={points(dest[dest$shortlist,3:4], pch=24); axis(1); axis(2); 
                  text(dest[dest$shortlist,3:4], pos=1, labels=dest[dest$shortlist,2], cex=0.7);
                })
   if (plotflag==1) {
     dev.copy2eps(file=paste0(dirs$outdir,"/map5f_model1.eps"))
     dev.copy2pdf(file=paste0(dirs$outdir,"/map5f_model1.pdf"))
   }
   dev.off()
}

  # Template for m1data for counterfactual predictions
  m1data_template<-m1data[1,]
  save(m1data_template,file=paste0(dirs$newdatadir,"/m1data_template.RData"))
  m2data<- data.frame(location_value,m1data)
  save(m2data,file=paste0(dirs$datadir,"/m2data1.RData"))
  save(m1data,file=paste0(dirs$datadir,"/m1data.RData"))
}
