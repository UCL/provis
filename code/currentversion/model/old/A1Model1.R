# Estimating "Model 1" using B-splines
# log(price_{it})=a_{t}+b'z_{it}+g(e_i,n_i)+u_{it}
# Author: MM, LN
# Last modified: 2017/12/07

# TODO
#  1) allows for interactions between (latitude,longitude)

#-----------------------------------------------------------------
# Set working directory
# Define path names
#-----------------------------------------------------------------
host<-system("hostname",intern=TRUE)
if (host=="dh-230-mac.econ.ucl.ac.uk") {
  RootDir<-"/Users/uctpln0/Documents/research/hedonic/NIC"
} else if (host=="minelava") {
  RootDir<-"C:/a/research/hedonic/NIC"
} else if (host=="DH-G06-03") {
  RootDir<-"U:/NICProject"
} else if (host=="jake.local" | host=="vic.local") {
  RootDir<-"/home/uctpln0/hedonic/NIC"
} else {
  info_sys<-Sys.info()
  user<-info_sys["user"]
  if (user=="uctpln0") {
    RootDir<-"/home/uctpln0/hedonic/NIC"
  } else {
    RootDir<-"/Users/larsnesheim/Documents/research/hedonic/NIC"
  }
}


# Parameters    
CVFlag<-2       # 0 : No cross-validation on full sample, c.v. on sample1
#                 1 : cross-validation on full sample
#                 2 : construct b-spline basis by hand on full sample, c.v. on sample1
N1<-5000        # N1   = size of sample 1
N0<-10000       # N0   = size of fullsample used for estimation
                # N0   = 0 then use all data (excluding outliers)
region_id<-1    # 1 = CaMKOx
region_str<-"CaMKOx"

source(paste0(RootDir,"/code/model/A0LoadLibrary.R"))
source(paste0(RootDir,"/code/model/A0SetPath.R"))

# Drop data prior to StartDate
StartDate<-as.Date("2008-01-01")

# Load data
if (region_id==1) {
  m1data<-read.csv(paste0(DataDir,"/CaMKOx_m10.csv"))
}

# For now, drop missing (mostly new builds)
# TODO:   fix problem with missing msoa11cd and other codes for new builds
#    i1<-m1data$msoa11cd==""
#    knn(m1data$train[!i1],test,k1,)
ikeep<- m1data$msoa11cd!=""
m1data<-m1data[ikeep,]

# Only Keep variables listed in varlist.csv
name1<-as.matrix(read.csv(file=paste0(DataDir,"/model1_varlist.csv"),header=T,as.is=T))
m1data<-m1data[,name1]

m1data$greenbelt[is.na(m1data$greenbelt)]<-0

# clean floor levels
rawlevels<-levels(m1data$floor_level)
m1data$cleanfloor<-"Ground floor"
m1data$cleanfloor[m1data$floor_level==rawlevels[9]]<-"1st floor"
m1data$cleanfloor[m1data$floor_level==rawlevels[10]]<-"2nd floor"
m1data$cleanfloor[m1data$floor_level==rawlevels[11]]<-"3rd floor"
for (i1 in c(1:8,12:17)) {
  m1data$cleanfloor[m1data$floor_level==rawlevels[i1]]<-"4th floor or higher"
}
m1data$cleanfloor[m1data$floor_level==rawlevels[21]]<-"Mid floor"
m1data$cleanfloor[m1data$floor_level==rawlevels[24]]<-"Top floor"
m1data$cleanfloor[m1data$floor_level==rawlevels[22]]<-"Unknown"
m1data$cleanfloor[m1data$floor_level==rawlevels[23]]<-"Unknown"
m1data$cleanfloor<-as.factor(m1data$cleanfloor)

# Create time dummies
m1data$date<-as.Date(m1data$cleandate)
m1data<-m1data[m1data$date>=StartDate,]  # drop dates prior to StartDate
m1data$year<-as.factor(year(m1data$date))
m1data$month<-as.factor(month(m1data$date))
m1data<-cbind(m1data,interaction(m1data$year,m1data$month))
i1<-grep("interaction*",colnames(m1data))
colnames(m1data)[i1]<-"time_dum"

# create interaction (lat_long)
m1data<-cbind(m1data,m1data$latitude*m1data$longitude)
i1<-grep("m1data*",colnames(m1data))
colnames(m1data)[i1]<-"lat_long"

m1data$logprice<-log(m1data$pricepaid)

# Drop outliers in size and price
qprice <- quantile(m1data$pricepaid, probs=c(.001, .999),na.rm=TRUE)
qsize <-  quantile(m1data$total_floor_area,probs=c(.01,.99),na.rm=TRUE)
iFull <- m1data$pricepaid>qprice[1] & m1data$pricepaid<qprice[2] &
         m1data$total_floor_area>qsize[1] & m1data$total_floor_area<qsize[2]
m1data<-m1data[iFull,]
        
set.seed(534)
m1datasample1<-m1data[sample(nrow(m1data), N1), ]

# create random subsample
if (N0>0) {
  m1data<-m1data[sample(nrow(m1data),N0),]
}

# Create some heatmaps
# Full sample
attach(m1data)
resolution <- 0.03 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
map1 <- interp(x=longitude, y=latitude, z=(logprice-mean(logprice))/sd(logprice), 
            yo=seq(min(latitude),max(latitude),by=resolution), 
            xo=seq(min(longitude),max(longitude),by=resolution), duplicate="mean")

dest<-read.csv(paste0(DataDir,"/DestinationsV2.csv"),header=TRUE)
dest_coord<-SpatialPoints(dest[,1:2])

filled.contour(map1, color.palette=terrain.colors, 
  plot.title={
  title(xlab="Longitude",cex.lab=1)
  mtext("Latitude",2,cex=1,line=3,las=0)
  mtext("Standardized log price",4,cex=1,line=0.8,las=0)
}, plot.axes={points(dest[20,1:2], pch=24); points(dest[22,1:2], pch=24);
  points(dest[23,1:2],pch=24); axis(1); axis(2); 
 text(dest[20,1:2], pos=1, labels="Cambridge", cex=0.7);
  text(dest[23,1:2], pos=1, labels="Milton Keynes", cex=0.7);
  text(dest[22,1:2], pos=1, labels="Oxford", cex=0.7);
  })

dev.copy(pdf,paste0(OutDir,'/map1_model1.pdf'))
dev.off()

# Reduced sample
attach(m1datasample1)
resolution <- 0.03 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
map2 <- interp(x=longitude, y=latitude, z=(logprice-mean(logprice))/sd(logprice), 
               yo=seq(min(latitude),max(latitude),by=resolution), 
               xo=seq(min(longitude),max(longitude),by=resolution), duplicate="mean")
filled.contour(map2, color.palette=terrain.colors, 
               plot.title={
                 title(xlab="Longitude",cex.lab=1)
                 mtext("Latitude",2,cex=1,line=3,las=0)
                 mtext("Standardized log price",4,cex=1,line=0.8,las=0)
               }, plot.axes={points(dest[20,1:2], pch=24); points(dest[22,1:2], pch=24);
                 points(dest[23,1:2],pch=24); axis(1); axis(2); 
                 text(dest[20,1:2], pos=1, labels="Cambridge", cex=0.7);
                 text(dest[23,1:2], pos=1, labels="Milton Keynes", cex=0.7);
                 text(dest[22,1:2], pos=1, labels="Oxford", cex=0.7);
               })

dev.copy(pdf,paste0(OutDir,'/map2_model1.pdf'))
dev.off()

# OLS benchmark
m1ols1<-lm(logprice~propertytype+newbuild+tenure+total_floor_area,m1data)
m1ols2<-lm(logprice~propertytype+newbuild+tenure+total_floor_area+cleanfloor,m1data)
m1ols3<-lm(logprice~year+propertytype+newbuild+tenure+total_floor_area,m1data)
m1ols4<-lm(logprice~time_dum+propertytype+newbuild+tenure+total_floor_area
            +latitude+longitude,m1data)
m1ols5<-lm(logprice~year+propertytype+newbuild+tenure+total_floor_area
                  +latitude+longitude,m1data)
m1ols6<-lm(logprice~time_dum+propertytype+newbuild+tenure+total_floor_area
              +latitude+longitude+lat_long,m1data)

stargazer(m1ols1, m1ols2,m1ols3, m1ols4, m1ols5, m1ols6,
          title="OLS Results", align=TRUE,out.header=TRUE,
          out=paste0(OutDir,"/OLS_table1.tex"),
          omit=c("time*","year*")) 

# Use sample 
m1ols1<-lm(logprice~propertytype+newbuild+tenure+total_floor_area,m1datasample1)
m1ols2<-lm(logprice~propertytype+newbuild+tenure+total_floor_area+cleanfloor,m1datasample1)
m1ols3<-lm(logprice~year+propertytype+newbuild+tenure+total_floor_area,m1datasample1)
m1ols4<-lm(logprice~time_dum+propertytype+newbuild+tenure+total_floor_area
           +latitude+longitude,m1datasample1)
m1ols5<-lm(logprice~year+propertytype+newbuild+tenure+total_floor_area
           +latitude+longitude,m1datasample1)
m1ols6<-lm(logprice~time_dum+propertytype+newbuild+tenure+total_floor_area
           +latitude+longitude+lat_long,m1datasample1)

stargazer(m1ols1, m1ols2,m1ols3, m1ols4, m1ols5, m1ols6,
          title="OLS Results", align=TRUE,out.header=TRUE,
          font.size="tiny",no.space=T,column.sep.width="2pt",
          out=paste0(OutDir,"/OLS_table2.tex"),
          omit=c("time*","year*")) 

# Main
m1spline1<-crs(logprice~time_dum+propertytype+newbuild+tenure+total_floor_area
              +latitude+longitude,
              data=m1datasample1,basis="additive",kernel=FALSE, cv="nomad",
              degree.max=30, segments.max=50)

save(m1spline1,file=paste0(OutDir,"/model1_sample1.RData"))

splinehat1<-matrix(predict(m1spline1,newdata=m1data))

resolution <- 0.03 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
map3 <- interp(x=m1data$longitude, y=m1data$latitude, z=(splinehat1-mean(splinehat1))/sd(splinehat1), 
               yo=seq(min(m1data$latitude),max(m1data$latitude),by=resolution), 
               xo=seq(min(m1data$longitude),max(m1data$longitude),by=resolution), duplicate="mean")
filled.contour(map3, color.palette=terrain.colors, 
               plot.title={
                 title(xlab="Longitude",cex.lab=1)
                 mtext("Latitude",2,cex=1,line=3,las=0)
                 mtext("Standardized predicted log price",4,cex=1,line=0.8,las=0)
               }, plot.axes={points(dest[20,1:2], pch=24); points(dest[22,1:2], pch=24);
                 points(dest[23,1:2],pch=24); axis(1); axis(2); 
                 text(dest[20,1:2], pos=1, labels="Cambridge", cex=0.7);
                 text(dest[23,1:2], pos=1, labels="Milton Keynes", cex=0.7);
                 text(dest[22,1:2], pos=1, labels="Oxford", cex=0.7);
               }, nlevels=20)
dev.copy(pdf,paste0(OutDir,'/map3_model1.pdf'))
dev.off()

plotdata<-plot(m1spline1,mean=TRUE,ci=TRUE,plot.behavior="data")

matplot(plotdata[[5]][,1],plotdata[[5]][,-1],
        xlab="Floor area",ylab="Conditional mean",
        lty=c(1,2,2),col=c(1,2,2),type="l")
dev.copy(pdf,paste0(OutDir,'/area_model1.pdf'))
dev.off()

matplot(plotdata[[6]][,1],plotdata[[6]][,-1],
        xlab="Latitude",ylab="Conditional mean",
        lty=c(1,2,2),col=c(1,2,2),type="l")
dev.copy(pdf,paste0(OutDir,'/lat_model1.pdf'))
dev.off()

matplot(plotdata[[7]][,1],plotdata[[7]][,-1],
        xlab="Longitude",ylab="Conditional mean",
        lty=c(1,2,2),col=c(1,2,2),type="l")
dev.copy(pdf,paste0(OutDir,'/lon_model1.pdf'))
dev.off()

# Recovering g(e,n)
m1splinecoef<-matrix(m1spline1$model.lm$coefficients)
m1splinem<-m1spline1$model.lm$model
m1splinex<-m1splinem$P
m1splinex<-cbind(rep(1,dim(m1splinex)[1]),m1splinex)
hatvals<-m1splinex %*% m1splinecoef # check if you get the same fitted values
hatvals==m1spline1$fitted.values

# Remember the order of variables matters in the crs function
# Here we assume that floor area is ALWAYS before latitude and longitude
sp_deg_area<-m1spline1$degree[1]
sp_seg_area<-m1spline1$segments[1]
sp_deg_lat<-m1spline1$degree[2]
sp_seg_lat<-m1spline1$segments[2]
sp_deg_lon<-m1spline1$degree[3]
sp_seg_lon<-m1spline1$segments[3]

location_value<-m1splinex[,(sp_deg_area+sp_seg_area+1):(sp_deg_area+sp_seg_area+sp_deg_lat+sp_seg_lat+sp_deg_lon+sp_seg_lon-2)] %*% m1splinecoef[(sp_deg_area+sp_seg_area+1):(sp_deg_area+sp_seg_area+sp_deg_lat+sp_seg_lat+sp_deg_lon+sp_seg_lon-2)]
plot(density(location_value), main=expression(g(e[i],n[i])))
dev.copy(pdf,paste0(OutDir,'/density_g_en.pdf'))
dev.off()

std_lv<-(location_value-mean(location_value))/sd(location_value)

attach(m1datasample1)
resolution <- 0.01 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
map4 <- interp(x=longitude, y=latitude, z=std_lv, 
               yo=seq(min(latitude),max(latitude),by=resolution), 
               xo=seq(min(longitude),max(longitude),by=resolution), duplicate="mean")
filled.contour(map4, color.palette=heat.colors, 
               plot.title={
                 title(xlab="Longitude",cex.lab=1)
                 mtext("Latitude",2,cex=1,line=3,las=0)
                 mtext("Standardized location value",4,cex=1,line=0.8,las=0)
               }, plot.axes={points(dest[20,1:2], pch=24); points(dest[22,1:2], pch=24);
                 points(dest[23,1:2],pch=24); axis(1); axis(2); 
                 text(dest[20,1:2], pos=1, labels="Cambridge", cex=0.7);
                 text(dest[23,1:2], pos=1, labels="Milton Keynes", cex=0.7);
                 text(dest[22,1:2], pos=1, labels="Oxford", cex=0.7);
               }, nlevels=20)

dev.copy(pdf,paste0(OutDir,'/map4_model1.pdf'))
dev.off()

# Full sample estimation
attach(m1data)

if (CVFlag==0) {
  # Skip cross-validation for full sample
  # use results (degree,segment,lambda,include) from m1spline1
  m1spline0<-crs(logprice~year+propertytype+newbuild+tenure+total_floor_area
                 +latitude+longitude,
                 data=m1data,basis="additive", kernel=FALSE,cv="none",
                 degree=m1spline1$degree,segments=m1spline1$segments,
                 lambda=m1spline1$lambda,include=m1spline1$include)
} else if (CVFlag==1) { 
  # Use Cross-validation for full sample
  m1spline0<-crs(logprice~year+propertytype+newbuild+tenure+total_floor_area
                           +latitude+longitude,
                           data=m1data,basis="additive", kernel=FALSE, cv="nomad",
                           degree.max=10, segments.max=50)
  
} else if (CVFlag==2) {
  # Set up splines
  degree<-m1spline1$degree
  segments<-m1spline1$segments
  knots<-list( quantile(m1data$total_floor_area,probs=seq(0.1,0.9,length.out=segments[1])), 
               quantile(m1data$latitude,probs=seq(0.1,0.9,length.out=segments[2])),
               quantile(m1data$longitude,probs=seq(0.1,0.9,length.out=segments[3])))
  
  m1data$bSize<-bSpline(m1data$total_floor_area,knots=knots[[1]],degree=degree[1],intercept=FALSE)
  m1data$bLat<-bSpline(m1data$latitude,knots=knots[[2]],degree=degree[2],intercept=FALSE)
  m1data$bLon<-bSpline(m1data$longitude,knots=knots[[3]],degree=degree[3],intercept=FALSE)

  # Estimate model  
  m1spline0<-lm(logprice~year+propertytype+newbuild+tenure
                          +bSize+bLat+bLon,
                          data=m1data)
  # Save spline details
  m1spline0$knots<-knots
  m1spline0$degree<-degree
  m1spline0$segments<-segments
  
}

m1spline0$CVFlag<-CVFlag
save(m1spline0,file=paste0(OutDir,"/m1spline0.RData"))

if (CVFlag<2) {
  plotdata_full<-plot(m1spline0,mean=TRUE,ci=TRUE,plot.behavior="data")

  matplot(plotdata_full[[5]][,1],plotdata_full[[5]][,-1],
          xlab="Floor area",ylab="Conditional mean",
          lty=c(1,2,2),col=c(1,2,2),type="l")
  dev.copy(pdf,paste0(OutDir,'/area_model1_full.pdf'))
  dev.off()

  matplot(plotdata_full[[6]][,1],plotdata_full[[6]][,-1],
          xlab="Latitude",ylab="Conditional mean",
          lty=c(1,2,2),col=c(1,2,2),type="l")
  dev.copy(pdf,paste0(OutDir,'/lat_model1_full.pdf'))
  dev.off()

  matplot(plotdata_full[[7]][,1],plotdata_full[[7]][,-1],
          xlab="Longitude",ylab="Conditional mean",
          lty=c(1,2,2),col=c(1,2,2),type="l")
  dev.copy(pdf,paste0(OutDir,'/lon_model1_full.pdf'))
  dev.off()

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
  ilat<-grep("bLat",names(m1spline0$coefficients))
  ilon<-grep("bLon",names(m1spline0$coefficients))
  
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
  
  # Recovering g(e,n)
  location_value<-logprice2+logprice3
}
    
plot(density(location_value), main=expression(g(e[i],n[i])))
dev.copy(pdf,paste0(OutDir,'/density_g_en_full.pdf'))
dev.off()

std_lv<-(location_value-mean(location_value))/sd(location_value)

resolution <- 0.001 # you can increase the resolution by decreasing this number (warning: the resulting dataframe size increase very quickly)
map5f <- interp(x=longitude, y=latitude, z=std_lv, 
               yo=seq(min(latitude),max(latitude),by=resolution), 
               xo=seq(min(longitude),max(longitude),by=resolution), duplicate="mean")
filled.contour(map5f, color.palette=heat.colors, 
               plot.title={
                 title(xlab="Longitude",cex.lab=1)
                 mtext("Latitude",2,cex=1,line=3,las=0)
                 mtext("Standardized location value",4,cex=1,line=0.8,las=0)
               }, plot.axes={points(dest[20,1:2], pch=24); points(dest[22,1:2], pch=24);
                 points(dest[23,1:2],pch=24); axis(1); axis(2); 
                 text(dest[20,1:2], pos=1, labels="Cambridge", cex=0.7);
                 text(dest[23,1:2], pos=1, labels="Milton Keynes", cex=0.7);
                 text(dest[22,1:2], pos=1, labels="Oxford", cex=0.7);
               })

dev.copy(pdf,paste0(OutDir,'/map5f_model1.pdf'))
dev.off()

### Save location value
m1data_template<-m1data[1,]
save(m1data_template,file=paste0(NewDataDir,"/","m1data_template.RData"))
m2data<- data.frame(location_value,m1data)
write.csv(m2data,file=paste0(DataDir,"/m2data0.csv"))
save(m1data,file=paste0(DataDir,"/m1data.RData"))
