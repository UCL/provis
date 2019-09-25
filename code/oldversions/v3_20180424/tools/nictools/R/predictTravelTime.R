#' predictTravelTime(origin,dest,region_id,TravelDir,travelmode,drop_outlier)
#' This function predicts travel times from orgin to dest, for region_id
#'
#' @param origin          data on (longitude,latitude) for origins
#' @param dest            destinations with (longitude,latitude)
#' @param region_id       id number for region between 1 and 11
#' @param TravelDir       directory containing travel model results
#' @param travelmodel     c("drive","trans")
#' @param drop_outlier    TRUE to drop top 0.001 outliers
#' @param basis           c("glp","tensor","cheb"): corresponding traveltime results must exist
#' @return origin         Add predicted travel times to origin
#' @keywords travel time
#' @export
#' @examples
#' newdata <-predictTravelTime(origin,dest,region_id,TravelDir,travelmode="drive",drop_outlier=TRUE,
#'                             "glp")
#' newdata <-predictTravelTime(origin,dest,region_id,TravelDir,travelmode="trans",drop_outlier=FALSE,
#'                             "cheb")
predictTravelTime<- function(origin,dest,region_id,TravelDir,travelmode="drive",drop_outlier=TRUE,
                             basis="glp") {

# Load travel prediction models
if (travelmode=="drive") {
  # load drivemodel
  load(file=paste0(TravelDir,"/",basis,"_drive_model_",as.character(region_id),".RData"))
} else if (travelmode=="trans") {
  # load transmodel
  load(file=paste0(TravelDir,"/",basis,"_transit_model_",as.character(region_id),".RData"))
}

# Make predictions: travel times to dest

#colnames(dest)<-c("name","shortname","longitude","latitude","shortlist")

# Stub names for new variables
timenames<-vector(mode="character",length=nrow(dest))

# data used to predict travel times
tempdata<-origin[,c("longitude","latitude")]
colnames(tempdata)[1:2]<-c("origin_x","origin_y")

n<-ncol(origin)

# Predict travel times for each destination
for (i1 in 1:nrow(dest)) {
  if (travelmode=="drive") {
    timenames[i1]<-paste0("drive_",dest$shortname[i1])
  } else if (travelmode=="trans") {
    timenames[i1]<-paste0("trans_",dest$shortname[i1])
  }

  tempdata$dest_x<-dest[i1,"longitude"]
  tempdata$dest_y<-dest[i1,"latitude"]

  n<-n+1
  print(paste0("Predicting ",timenames[i1],": ",
               as.character(i1)," of ",as.character(length(timenames))))

  if (travelmode=="drive") {
    # Predict drive time
    if (basis=="glp" | basis=="tensor") {
      x<-predict(drivemodel,newdata=tempdata)
    } else if (basis=="cheb") {
      # create tensor product of Chebyshev polynomials
      if (drivemodel$distflag==0) {
        xx<-tensorCheb(tempdata[,c("origin_x","origin_y","dest_x","dest_y")],drivemodel$k,
                       drivemodel$lo,drivemodel$hi)
      } else if (drivemodel$distflag==1) {
        tempdata$distance <- sqrt( (tempdata$origin_x-tempdata$dest_x)^2
                          +(tempdata$origin_y-tempdata$dest_y)^2)
        tempdata$theta    <- acos((tempdata$dest_x-tempdata$origin_x)/tempdata$distance)
        xx<-tensorCheb(tempdata[,c("origin_x","origin_y","distance","theta")],drivemodel$k,
                       drivemodel$lo,drivemodel$hi)
      }
      x<- as.matrix(xx$x) %*% as.matrix(drivemodel$b)
      if (drivemodel$logflag==1) {
        x<-exp(x)
      } else if (drivemodel$logflag==2) {
        x<-drivemodel$drivelo + (drivemodel$drivehi-drivemodel$drivelo) *
           exp(x)/(1+exp(x))
      }
    }
  } else if (travelmode=="trans") {
    # Predict transit time
    if (basis=="glp" | basis=="tensor") {
      x<-predict(transmodel,newdata=tempdata)
    } else if (basis=="cheb") {
      if (transmodel$distflag==0) {
        # create tensor product of Chebyshev polynomials
        xx<-tensorCheb(tempdata[,c("origin_x","origin_y","dest_x","dest_y")],transmodel$k,
                       transmodel$lo,transmodel$hi)
      } else if (transmodel$distflag==1) {
        tempdata$distance <- sqrt( (tempdata$origin_x-tempdata$dest_x)^2
                                   +(tempdata$origin_y-tempdata$dest_y)^2)
        tempdata$theta    <- acos((tempdata$dest_x-tempdata$origin_x)/tempdata$distance)
        xx<-tensorCheb(tempdata[,c("origin_x","origin_y","distance","theta")],transmodel$k,
                       transmodel$lo,transmodel$hi)
      }
      x<- as.matrix(xx$x) %*% as.matrix(transmodel$b)
      if (transmodel$logflag==1) {
        x<-exp(x)
      } else if (transmodel$logflag==2) {
        x<- transmodel$translo + (transmodel$transhi-transmodel$translo) *
            exp(x)/(1+exp(x))
      }
    }
  }

  # Drop outliers
  x[x<0] <- NA
  if (length(x)>=10000 & drop_outlier) {
    x999<-quantile(x,.999,na.rm=TRUE)
    x[x>=x999]<-NA
  }
  origin$x<-NA
  origin$x[!is.na(origin$latitude)]<-x
  colnames(origin)[n]<-timenames[i1]
}
return(origin)
}


