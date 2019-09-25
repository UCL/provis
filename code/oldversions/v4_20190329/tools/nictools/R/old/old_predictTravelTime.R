#' predictTravelTime(origin,dest,region_id,TravelDir)
#' This function predicts travel times from orgin to dest, for region_id
#' 
#' @param origin          data on (longitude,latitude) for origins
#' @param dest            destinations with (longitude,latitude)
#' @param region_id       id number for region between 1 and 11
#' @param TravelDir       directory containing travel model results
#' @param drop_outlier    TRUE to drop top 0.001 outliers
#' @return origin         Add predicted travel times to origin
#' @keywords travel time
#' @export
#' @examples
#' newdata <-predictTravelTime(origin,dest,region_id,TravelDir)
predictTravelTime<- function(origin,dest,region_id,TravelDir,drop_outlier) {

if (missing(drop_outlier)) { drop_outlier<-TRUE}

# Load travel prediction models  
load(file=paste0(TravelDir,"/glp_drive_model_",as.character(region_id),".RData"))  
drive_model<-z  
rm(z)
load(file=paste0(TravelDir,"/glp_transit_model_",as.character(region_id),".RData"))  
trans_model<-zz
rm(zz)
  
# Make predictions: travel times to dest

#colnames(dest)<-c("name","shortname","longitude","latitude","shortlist")

# Stub names for new variables
timenames<-vector(mode="character",length=2*nrow(dest))

# data used to predict travel times
tempdata<-origin[,c("longitude","latitude")]
colnames(tempdata)[1:2]<-c("origin_x","origin_y")

n<-ncol(origin)

# Predict travel times for each destination
for (i1 in 1:nrow(dest)) {
  timenames[i1]<-paste0("drive_",dest$shortname[i1])
  timenames[nrow(dest)+i1]<-paste0("trans_",dest$shortname[i1])

  tempdata$dest_x<-dest[i1,"longitude"]
  tempdata$dest_y<-dest[i1,"latitude"]
  
  n<-n+1
  print(paste0("Predicting ",timenames[i1],": ",
               as.character(2*(i1-1)+1)," of ",as.character(length(timenames))))
  # Predict drive time
  assign("x",predict(drive_model,newdata=tempdata))
  
  # Drop outliers
  x[x<0] <- NA
  if (length(x)>=10000 & drop_outlier) {
    x999<-quantile(x,.999,na.rm=TRUE)
    x[x>=x999]<-NA
  }
  origin$x<-NA
  origin$x[!is.na(origin$latitude)]<-x
  colnames(origin)[n]<-timenames[i1]
  
  n<-n+1
  print(paste0("Predicting ",timenames[nrow(dest)+i1],": ",
               as.character(2*(i1-1)+2)," of ",as.character(length(timenames))))
  # Predict transit time
  assign("x",predict(trans_model,newdata=tempdata))
  
  # Drop outliers
  x[x<0] <- NA
  if (length(x)>=10000 & drop_outlier) {
    x999<-quantile(x,.999,na.rm=TRUE)
    x[x>=x999]<-NA
  }
  origin$x<-NA
  origin$x[!is.na(origin$latitude)]<-x
  colnames(origin)[n]<-timenames[nrow(dest)+i1]
}
return(origin)
}


