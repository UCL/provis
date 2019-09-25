# Travel time models for different regions
# Author: MM, LN
# Last modified: 2017/12/27

dirs<- B2SetPath(RootDir,CodeDir,region_id=0)
DataDir<-paste0(dirs$datadir,"/TravelTime/data")
setwd(paste0(CodeDir,"/TravelTime"))

# Transformations of coordinates and of dependent variable
# distanceflag
#    0 : y = f1(x1,y1,x2,y2)
#    1 : y = f2(x1,y1,distance,theta)
# logflag 
#    0 : y = traveltime
#    1 : y = log(traveltime)
#    2 : y = log(traveltime-ylo) - log(yhi-traveltime)
#        ylo = 0.
#        yhi = 1.5*max(y)
distanceflag <- 1  
logflag      <- 2

if (host=="rserver.econ.ucl.ac.uk") {
  matlab <- "matlab -nodisplay -r "
  plotflag<-2  # 0 no plots. 1=plot on screen. 2=plot to file only
} else if (host=="dh-230-mac.econ.ucl.ac.uk") {
  matlab<-"/Applications/MATLAB_R2017a.app/bin/matlab -nodisplay -r "
  plotflag<-1  # 0 no plots. 1=plot on screen. 2=plot to file only
} else if (host=="minelava") {
  matlab<-"C:\\Program Files\\MATLAB\\R2014a\\bin\\matlab -nodesktop -minimize -r "
  plotflag<-1  # 0 no plots. 1=plot on screen. 2=plot to file only
} else {
  if (user=="larsnesheim") {
    matlab<-"/Applications/MATLAB_R2015a.app/bin/matlab -nodisplay -r "
    plotflag<-1  # 0 no plots. 1=plot on screen. 2=plot to file only
  }
}
#-----------------------------------------------------------------
# Install packages (crs)
#-----------------------------------------------------------------
library(crs)
library(geosphere)
library(ggplot2)
library(sp)
library(nictools)

# Create bounding box for spatial data
wgs.84    <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
ukgrid = "+init=epsg:27700"
#-----------------------------------------------------------------
# Open files 
#-----------------------------------------------------------------
nreg<-11
for (i in 1:11) {
fname<-paste0("00_ttsample_",i,".csv")
vname<-paste0("ttsample_",i)
assign(vname, read.csv(paste0(DataDir,"/",fname), header=TRUE))
}

#-----------------------------------------------------------------
# Estimate models (crs)
#-----------------------------------------------------------------
samplenames<-ls(pattern="ttsample")

basis<-"cheb"  # c("glp","tensor","cheb")
for (a in samplenames) {
  region<-strsplit(a,'_')[[1]][2]
  currentsample<-get(a)
  attach(currentsample)
  
  origin<-SpatialPoints(currentsample[,c("origin_x","origin_y")],CRS(wgs.84))
  origin<-coordinates(spTransform(origin,CRS(ukgrid)))
  dest  <-SpatialPoints(currentsample[,c("dest_x","dest_y")],CRS(wgs.84))
  dest  <-coordinates(spTransform(dest,CRS(ukgrid)))
  
  currentsample$distance <- sqrt( (origin[,1]-dest[,1])^2 +(origin[,2]-dest[,2])^2)
  currentsample$theta    <- acos((dest[,1]-origin[,1])/currentsample$distance)
  currentsample$distance <-currentsample$distance/1000

  # Remove missing values
  set.seed(43764)
  n<-nrow(currentsample)
  n1<-n
  iFull_drv<- !is.na(drive_time)
  iFull_tr <- !is.na(transit_time)
  if (n1<n) {
    iFull_drv<- sample(c(1:n)[iFull_drv],size=n1,replace=FALSE)
    iFull_tr <- sample(c(1:n)[iFull_tr],size=n1,replace=FALSE)
  }
  regno<-as.numeric(strsplit(a, "ttsample_")[[1]][2])
  drv_name<-paste0(basis,"_drive_model_",regno)
  tr_name<-paste0(basis,"_transit_model_",regno)

  if (basis=="glp" | basis=="tensor") {
    # Transformation of dependent variable and transformation of coordinates
    # Transform dependent variable
    if (logflag==0) {
      currentsample$y1<-drive_time
      currentsample$y2<-transit_time
    } else if (logflag==1) {
      currentsample$y1<-log(drive_time)
      currentsample$y2<-log(transit_time)
    } else if (logflag==2) {
      y1lo<-0
      y1hi<-1.5*max(drive_time[!is.na(drive_time)])
      currentsample$y1 <- log((drive_time-y1lo)/(y1hi-drive_time))
      y2lo<-0
      y2hi<-1.5*max(transit_time[!is.na(transit_time)])
      currentsample$y2<- log((transit_time-y2lo)/(y2hi-transit_time))
    }
    
    # Transform coordinates
    if (distanceflag==0) {
      assign(drv_name,try(crs(y1~origin_x+origin_y+dest_x+dest_y, basis=basis,
                              kernel=FALSE, cv="nomad", degree.max=10, segments.max=30, 
                              data=currentsample[iFull_drv,])))
      assign(tr_name,try(crs(y2~origin_x+origin_y+dest_x+dest_y, basis=basis, 
                             kernel=FALSE, cv="nomad", degree.max=10, segments.max=30, 
                             data=currentsample[iFull_tr,])))
    } else if (distanceflag==1) {  
      assign(drv_name,try(crs(y1~origin_x+origin_y+distance+theta, basis=basis,
                              kernel=FALSE, cv="nomad", degree.max=10, segments.max=30, 
                              data=currentsample[iFull_drv,])))
      assign(tr_name,try(crs(y2~origin_x+origin_y+distance+theta, basis=basis, 
                             kernel=FALSE, cv="nomad", degree.max=10, segments.max=30, 
                             data=currentsample[iFull_tr,])))
    }    
    drive<-get(drv_name)
    trans<-get(tr_name)
    
    drive$logflag      <- logflag
    drive$distanceflag <- distanceflag
    drive$ylo          <- y1lo
    drive$yhi          <- y1hi
    
    trans$logflag      <- logflag
    trans$distanceflag <- distanceflag
    trans$ylo         <- y2lo
    trans$yhi         <- y2hi
    
    save(drive, file=paste0(dirs$traveldir,"/",drv_name,".RData"))
    save(trans, file=paste0(dirs$traveldir,"/",tr_name,".RData"))
    
    # Plot densities of predictions
    currentsample$drive_timehat   <- NA
    currentsample$transit_timehat <- NA

    y1hat<-predict(drive,data=currentsample)
    y2hat<-predict(trans,data=currentsample)
    currentsample$drive_timehat<-NA
    currentsample$trans_timehat<-NA
    if (logflag==0) {
      currentsample$drive_timehat<-y1hat
      currentsample$transit_timehat<-y2hat
    } else if (logflag==1) {
      currentsample$drive_timehat<-exp(y1hat)
      currentsample$transit_timehat<-exp(y2hat)
    } else if (logflag==2) {
      currentsample$drive_timehat <- drive$ylo + (drive$yhi-drive$ylo) * exp(y1hat)/(1+exp(y1hat))
      currentsample$trans_timehat <- trans$ylo + (trans$yhi-trans$ylo) * exp(y2hat)/(1+exp(y2hat))
    }
    if (plotflag>0) {
      if (plotflag==2) {
        tempfile<-paste0(dirs$traveldir,"/traveltimes",as.character(region),".pdf")
        pdf(file=tempfile)
      }
      plot(density(currentsample$drive_time[iFull_drv]))
      lines(density(currentsample$drive_timehat[iFull_drv]),col="red")
      if (plotflag==2) dev.copy(pdf,tempfile)
      
      plot(density(currentsample$transit_time[iFull_tr]))
      lines(density(currentsample$trans_timehat[iFull_tr]),col="red")
      if (plotflag==2) dev.copy(pdf,tempfile)
      
      plot(currentsample$distance[iFull_drv],currentsample$drive_time[iFull_drv],pch=".")
      points(currentsample$distance[iFull_drv],currentsample$drive_timehat[iFull_drv],
             pch=".",col="red")
      if (plotflag==2) dev.copy(pdf,tempfile)
      
      plot(currentsample$distance[iFull_drv],currentsample$transit_time[iFull_drv],pch=".")
      points(currentsample$distance[iFull_drv],currentsample$trans_timehat[iFull_drv],
             pch=".",col="red")
      if (plotflag==2) dev.copy(pdf,tempfile)
      if (plotflag==2) dev.off()
    }
    
  } else if (basis=="cheb") {
    # use tensor product of Chebyshev polynomials
    k<-c(5,5)      # k[1]-1 = order of highest polynomial
                    # k[2]^3 * k[1] = dimension of subset of tensor product
    lambda<-0.0001  # ridge regression parameter to fit linear model to polynomials
    if (!exists('matlab')) {
      # path to matlab server must be defined at top on each server.
      print('Error in basis=cheb. matlab path is not set on this server.')
      stop
    }
    # Strings to be passed to matlab
    c1    <- paste0('"DataDir = \'',DataDir,'\'; ')
    c2    <- paste0('OutDir = \'',dirs$traveldir,'\'; ')
    c3    <- paste0('k = [',as.character(k[1]),';',as.character(k[2]),'];')
    c4    <- paste0('travelmodel(',region,',',
                    'k,',
                    as.character(lambda),',',
                    as.character(logflag),',',
                    as.character(distanceflag),',',
                    as.character(plotflag),',DataDir,OutDir); exit;"')
    xx<-  paste0(matlab,c1,c2,c3,c4)
    # call matlab to run linear regression and ridge regression
    system(xx)
    # load parameters from matlab linear regression
    bdrive<-read.csv(paste0(dirs$traveldir,'/drive',region,'.csv'),header=FALSE)
    btrans<-read.csv(paste0(dirs$traveldir,'/transit',region,'.csv'),header=FALSE)
    xlohi_drive<-read.csv(paste0(dirs$traveldir,'/xlohi_drive',region,'.csv'),header=FALSE)
    xlohi_trans<-read.csv(paste0(dirs$traveldir,'/xlohi_transit',region,'.csv'),header=FALSE)
    
    # create tensor product of Chebyshev polynomials
    if (distanceflag==0) {
      xdrive<-tensorCheb(currentsample[iFull_drv,c("origin_x","origin_y","dest_x","dest_y")],k,
                         xlohi_drive[,1],xlohi_drive[,2])        
      xtrans<-tensorCheb(currentsample[iFull_tr,c("origin_x","origin_y","dest_x","dest_y")],k,
                         xlohi_trans[,1],xlohi_trans[,2])        
    } else if (distanceflag==1) {
      currentsample$distance <- sqrt( (origin_x-dest_x)^2 +(origin_y-dest_y)^2)
      currentsample$theta    <- acos((dest_x-origin_x)/currentsample$distance)
#      currentsample$distance <-currentsample$distance
      
      xdrive<-tensorCheb(currentsample[iFull_drv,c("origin_x","origin_y","distance","theta")],k,
                         xlohi_drive[,1],xlohi_drive[,2])        
      xtrans<-tensorCheb(currentsample[iFull_tr,c("origin_x","origin_y","distance","theta")],k,
                         xlohi_trans[,1],xlohi_trans[,2])        
    }
    # predicted drivetime and predicted transit time 
    drivehat<- as.matrix(xdrive$x) %*% as.matrix(bdrive)
    transhat<- as.matrix(xtrans$x) %*% as.matrix(btrans)
    drivelo<-0
    drivehi<-0
    translo<-0
    transhi<-0
    
    if (logflag==1) {
      drivehat <- exp(drivehat)
      transhat <- exp(transhat)
    } else if (logflag==2) {
      drivelo<-0
      drivehi<-1.1*max(drive_time[iFull_drv])
      drivehat <- drivelo + (drivehi-drivelo)*exp(drivehat)/(1+exp(drivehat))
      translo<-0
      transhi<-1.1*max(transit_time[iFull_tr])
      transhat <- translo + (transhi-translo)*exp(transhat)/(1+exp(transhat))
    }
    
    # Plot densities of predictions
    currentsample$drive_timehat   <- NA
    currentsample$transit_timehat <- NA
    currentsample$drive_timehat[iFull_drv]<-drivehat
    currentsample$transit_timehat[iFull_tr]<-transhat
    if (plotflag>0) {
      if (plotflag==2) {
        tempfile<-paste0(dirs$traveldir,"/traveltimes",as.character(region),".pdf")
        pdf(file=tempfile)
      }
      plot(density(currentsample$drive_time[iFull_drv]))
      lines(density(currentsample$drive_timehat[iFull_drv]),col="red")
      if (plotflag==2) dev.copy(pdf,tempfile)
      
      plot(density(currentsample$transit_time[iFull_tr]))
      lines(density(currentsample$transit_timehat[iFull_tr]),col="red")
      if (plotflag==2) dev.copy(pdf,tempfile)
      
      plot(currentsample$distance[iFull_drv],currentsample$drive_time[iFull_drv],pch=".")
      points(currentsample$distance[iFull_drv],currentsample$drive_timehat[iFull_drv],pch=".",col="red")
      if (plotflag==2) dev.copy(pdf,tempfile)
      if (plotflag==2) dev.off()
    }
    # Save results
    drivemodel<-list(bdrive,k,xdrive$lo,xdrive$hi,logflag,distanceflag,drivelo,drivehi)
    names(drivemodel)<-c("b","k","lo","hi","logflag","distflag","drivelo","drivehi")
    save(drivemodel,file=paste0(dirs$traveldir,'/',drv_name,'.RData'))
    transmodel<-list(btrans,k,xtrans$lo,xtrans$hi,logflag,distanceflag,translo,transhi)
    names(transmodel)<-c("b","k","lo","hi","logflag","distflag","translo","transhi")
    save(transmodel,file=paste0(dirs$traveldir,'/',tr_name,'.RData'))
  }            
  detach(currentsample)
}
