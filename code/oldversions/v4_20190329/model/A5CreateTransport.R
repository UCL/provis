#' A5CreateTransport(infrastructure_type,method,shapefile,multiplier,dirs)
#' Create data frames for new rail/road and predict prices
#' 
#' @param region              region id number
#' @param infrastructure_type "road" or "rail"
#' @param method              "newroute" or "newspeed" 
#' @param shapefile           filename containing route junction coordinates
#' @param multiplier          number between (0.5,1.5) to set new speed and travel time
#' @param speed               speed on new route, miles per hour
#' @param depvar              dependent variable. one of c("logvalue","value","boxcoxvalue")
#' @param dirs                list of directories
#' @param usemapdir           TRUE = look in mapdir for shapefile, FALSE = no directory added to path
#' @param travelmodelbasis    c("cheb","glp","tensor")
#' @return outdata            list containing (m1data_new,m2data_new)
#' @keywords new settlement
#' @export
#' @examples
#' newdata <- A5CreateTransport(region,infrastructure_type="road",method="newroute",
#'                              shapefile="file1.kml",multiplier=0.9,speed=50,
#'                              depvar="logvalue",dirs,usemapdir=TRUE,travelmodelbasis="cheb")
A5CreateTransport<-function(region,infrastructure_type,method,shapefile=NULL,multiplier,speed,depvar="logvalue",
                            dirs,usemapdir=TRUE,travelmodelbasis="cheb") {
  default_warn<-getOption("warn")
  options(warn=-1)
  dest<-read.csv(paste0(dirs$datadir,"/destinations",region,".csv"))
  load(paste0(dirs$datadir,"/m1data.RData"))
  load(paste0(dirs$datadir,"/m2data2.RData"))

  # set location value to one of c("logvalue","value","boxcoxvalue")
  m2data$location_value <- m2data[,grep(paste0("\\<",depvar),colnames(m2data))]
  
  # Convert prob_4band="" to prob_4band==NA
  i1<-(levels(m2data$prob_4band)=="")
  levels(m2data$prob_4band)[i1]<-NA
  
  # Drop rows either in m1data or m2data to ensure that 
  # nrow(m1data) = nrow(m2data)
  if (nrow(m1data)>nrow(m2data)) {
    n1<-nrow(m1data)
    n2<-nrow(m2data)
    set.seed(569472)
    i1<-sample(c(1:n1),size=n2)
    m1data<-m1data[i1,]
  } else if (nrow(m2data)>nrow(m1data)) {
    n1<=nrow(m1data)
    n2<-nrow(m2data)
    set.seed(569471)
    i1<-sample(c(1:n2),size=n1)
    m2data<-m2data[i1,]
  }
  m2data_old<-m2data
  if (method=="newspeed") {
    # update travel time
    if (infrastructure_type=="road") {
      ispline  <- grep("spline_drive",colnames(m2data))
      inew     <- grep("\\<drive_",colnames(m2data))
    } else if (infrastructure_type=="rail") {
      ispline <- grep("spline_trans",colnames(m2data))
      inew    <- grep("\\<trans_",colnames(m2data))
    }  
    ispline2   <- grep("\\<spline_avgtime",colnames(m2data))
    iavgtime   <- grep("\\<avgtime_",colnames(m2data))
    iprobdrive <- grep("\\<probdrive_",colnames(m2data))

    
    if (is.null(shapefile)) {
      # Change in travel time applies to whole region
      m2data[,inew] <- multiplier*m2data[,inew]
    } else {
      # Change in travel time applies only to subset of region
      # that intersects with polygon defined in shapefile
      if (usemapdir) {
        shapefile<-paste0(dirs$mapdir,"/",shapefile)
      }  
      layer1<-ogrListLayers(shapefile)
      speedregion<-readOGR(shapefile,layer=layer1)
      i1<-gIntersects(SpatialPoints(m2data[,c("longitude","latitude")],CRS(wgs.84)),
                      speedregion,byid=TRUE)
      m2data[i1[1,],inew]<-multiplier*m2data[i1[1,],inew]
    }
    
    # Drop all variables that are derived from travel times and that must be updated
    m2data<-m2data[,-c(ispline,ispline2,iavgtime,iprobdrive)]
    
    # Update average times
    # update (avgtime_*,driveprob_*)
    m2data<-ComputeAverageTime(m2data)
    
    load(file=paste0(dirs$outdir,"/m2dataspline.RData"))
    
    if (infrastructure_type=="road") {
      inew<-grep("drive_",m2dataspline$varlist)
      inew2<-grep("avgtime_",m2dataspline$varlist)
    } else if (infrastructure_type=="rail") {
      inew<-grep("trans_",m2dataspline$varlist)
      inew2<-grep("avgtime_",m2dataspline$varlist)
    }
    
    for (i1 in c(inew,inew2)) {
      m2data$tempB<-bSpline(m2data[,m2dataspline$varlist[i1]],
                            knots=m2dataspline$knots[[i1]],
                            degree=m2dataspline$degree[i1],intercept=FALSE)
      iTempB<-grep("tempB",colnames(m2data))
      names(m2data)[iTempB]<-paste0("spline_",m2dataspline$varlist[i1])
    }
  } else if (method=="newroute") {
    if (infrastructure_type=="road") {
      m2data<-A7NewTransportRoute(region,shapefile,travelmode="drive",speed,dirs,
                                  usemapdir,travelmodelbasis,m2data) 
      load(file=paste0(dirs$outdir,"/m2dataspline.RData"))
      idrive   <- grep("drive_",m2dataspline$varlist)
      iavgtime <- grep("avgtime_",m2dataspline$varlist)
      for (i1 in c(idrive,iavgtime)) {
        m2data$tempB<-bSpline(m2data[,m2dataspline$varlist[i1]],
                              knots=m2dataspline$knots[[i1]],
                              degree=m2dataspline$degree[i1],intercept=FALSE)
        iTempB<-grep("tempB",colnames(m2data))
        names(m2data)[iTempB]<-paste0("spline_",m2dataspline$varlist[i1])
      }
    } else if (infrastructure_type=="rail") {
      m2data<-A7NewTransportRoute(region,shapefile,travelmode="trans",speed,dirs,
                                  usemapdir,travelmodelbasis,m2data) 
      load(file=paste0(dirs$outdir,"/m2dataspline.RData"))
      itrans   <- grep("trans_",m2dataspline$varlist)
      iavgtime <- grep("avgtime_",m2dataspline$varlist)
      for (i1 in c(itrans,iavgtime)) {
        m2data$tempB<-bSpline(m2data[,m2dataspline$varlist[i1]],
                              knots=m2dataspline$knots[[i1]],
                              degree=m2dataspline$degree[i1],intercept=FALSE)
        itempB<-grep("tempB",colnames(m2data))
        names(m2data)[itempB]<-paste0("spline_",m2dataspline$varlist[i1])
      }
    }
  }
  
  outdata<-list(m1data,m2data,m2data_old)
  names(outdata)<-c("m1data_new","m2data_new","m2data_old")
  options(warn=default_warn)
  return(outdata)
}
