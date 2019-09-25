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
plotflag<-1     # 0 do not plot
                # 1 plot to screen then save
                # 2 plot directly to file, no screen created
datastub<-"nondom"  # c("m11","nondom")

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

for (r in 1:nregs) {
region_id<-regnames$region_id[r]    
region_str<-regnames$region_str[r] 
dirs<-B2SetPath(RootDir,CodeDir,region_id,datastub)

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
  #register_google(key = "", account_type = "premium", day_limit = 100000)
}
m1data<-GetTownNames(m1data,datastub)
  
# Create bounding box for region
bbx_str<-paste0("POLYGON((",min(m1data$longitude,na.rm=TRUE)," ",min(m1data$latitude,na.rm=TRUE),",",
                max(m1data$longitude,na.rm=TRUE)," ",min(m1data$latitude,na.rm=TRUE),",",
                max(m1data$longitude,na.rm=TRUE)," ",max(m1data$latitude,na.rm=TRUE),",",
                min(m1data$longitude,na.rm=TRUE)," ",max(m1data$latitude,na.rm=TRUE),",",
                min(m1data$longitude,na.rm=TRUE)," ",min(m1data$latitude,na.rm=TRUE),"))")
bbx<-readWKT(bbx_str,p4s=CRS(wgs.84),id=as.character(region_str))
region_bbx[[r]]<-bbx

iNoMiss<-complete.cases(m1data$rateablevalue, m1data$latitude, m1data$longitude, m1data$totalarea)
m1data<-m1data[iNoMiss,]

# Drop observatations with rateablevalue==0
#   These are vacant, under construction or otherwise have zero rateable value
#   however, economic value is not zero
izero<-m1data$rateablevalue>0
m1data<-m1data[izero,]

long_lat<-SpatialPoints(m1data[,c("longitude","latitude")],CRS(wgs.84))

# create interaction (lat_long)
m1data<-cbind(m1data,m1data$latitude*m1data$longitude)
i1<-grep("m1data*",colnames(m1data))
colnames(m1data)[i1]<-"lat_long"

m1data$logprice<-log(m1data$rateablevalue)

# Drop outliers in size and price
qprice <- quantile(m1data$rateablevalue, probs=c(.001, .999),na.rm=TRUE)
qsize <-  quantile(m1data$totalarea,probs=c(.01,.99),na.rm=TRUE)
iFull <- m1data$rateablevalue>qprice[1] & m1data$rateablevalue<qprice[2] &
         m1data$totalarea>qsize[1] & m1data$totalarea<qsize[2]
m1data<-m1data[iFull,]
        
set.seed(534)
m1datasample1<-m1data[sample(nrow(m1data), N1), ]

# create random subsample
if (N0>0 & N0<nrow(m1data)) {
  m1data<-m1data[sample(nrow(m1data),N0),]
}

# Create some heatmaps
# Full sample
attach(m1data)

# Main
if (LatLonFlag==0) {
  m1spline1<-crs(logprice~usecode+totalarea
              +latitude+longitude,
              data=m1datasample1,basis="additive",kernel=FALSE, cv="nomad",
              degree.max=30, segments.max=120)
} else if (LatLonFlag==1) {
  m1spline1<-crs(logprice~usecode+totalarea
                 +latitude+longitude+lat_long,
                 data=m1datasample1,basis="additive",kernel=FALSE, cv="nomad",
                 degree.max=50, segments.max=200)
}

save(m1spline1,file=paste0(dirs$outdir,"/model1_sample1.RData"))

splinehat1<-matrix(predict(m1spline1,newdata=m1data))

# Load destinations
DestinationFile<-paste0(dirs$datadir,"/destinations",region_id,"B.csv")
if (!file.exists(DestinationFile)) {
  dest<-B3CreateDestinations(region_id)
  write.csv(dest,DestinationFile,row.names=FALSE)
} else {
  dest<-read.csv(DestinationFile)  
}


 if (plotflag==1 | plotflag==2) {
   resolution <- 0.01 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
   map3 <- interp(x=m1data$longitude, y=m1data$latitude, z=(splinehat1-mean(splinehat1))/sd(splinehat1), 
                  yo=seq(min(m1data$latitude),max(m1data$latitude),by=resolution), 
                  xo=seq(min(m1data$longitude),max(m1data$longitude),by=resolution), duplicate="mean")
   if (plotflag==2) {
     pdf(file=paste0(dirs$outdir,"/map3_model1.pdf"))
   } 
   filled.contour(map3, color.palette=terrain.colors, 
                  plot.title={
                    title(xlab="Longitude",cex.lab=1)
                    mtext("Latitude",2,cex=1,line=3,las=0)
                    mtext("Standardized predicted log price",4,cex=1,line=0.8,las=0)
                  }, plot.axes={points(dest[dest$shortlist,3:4], pch=24);axis(1);axis(2); 
                    text(dest[dest$shortlist,3:4], pos=1, labels=dest[dest$shortlist,2], cex=0.7);
                  }, nlevels=20)
   if (plotflag==1) {
     dev.copy(pdf,paste0(dirs$outdir,"/map3_model1.pdf"))
   }
   dev.off()

  plotdata<-plot(m1spline1,mean=TRUE,ci=TRUE,plot.behavior="data")

  if (plotflag==2) {
    pdf(paste0(dirs$outdir,"/area_model1.pdf"))
  }
  matplot(plotdata[[2]][,1],plotdata[[2]][,-1],
          xlab="Floor area",ylab="Conditional mean",
          lty=c(1,2,2),col=c(1,2,2),type="l")
  if (plotflag==1) {
    dev.copy(pdf,paste0(dirs$outdir,"/area_model1.pdf"))
  }
  dev.off()

  if (plotflag==2) {
    pdf(paste0(dirs$outdir,"/lat_model1.pdf"))
  }
  matplot(plotdata[[3]][,1],plotdata[[3]][,-1],
          xlab="Latitude",ylab="Conditional mean",
          lty=c(1,2,2),col=c(1,2,2),type="l")
  if (plotflag==1) {
    dev.copy(pdf,paste0(dirs$outdir,"/lat_model1.pdf"))
  }
  dev.off()

  if (plotflag==2) {
    pdf(paste0(dirs$outdir,"/lon_model1.pdf"))
  }
  matplot(plotdata[[4]][,1],plotdata[[4]][,-1],
          xlab="Longitude",ylab="Conditional mean",
          lty=c(1,2,2),col=c(1,2,2),type="l")
  if (plotflag==1) {
    dev.copy(pdf,paste0(dirs$outdir,"/lon_model1.pdf"))
  }
  dev.off()
 }

# Full sample estimation
attach(m1data)

if (CVFlag==0) {
  # Skip cross-validation for full sample
  # use results (degree,segment,lambda,include) from m1spline1
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
