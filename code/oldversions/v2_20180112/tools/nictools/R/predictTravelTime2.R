#' predictTravelTime2(orig_dest,region_id,TravelDir)
#' This function predicts travel times from orgin to dest, for region_id
#' 
#' @param orig_dest       data on (longitude,latitude) for (origin,destination) pairs
#' @param destname       short name of destination
#' @param region_id       id number for region between 1 and 11
#' @param TravelDir       directory containing travel model results
#' @return traveltime     Add predicted travel times to origin
#' @keywords travel time
#' @export
#' @examples
#' traveltime <-predictTravelTime2(orig_dest,destname,region_id,TravelDir)
predictTravelTime2<- function(orig_dest,destname,region_id,TravelDir) {

  # Load travel prediction models  
  load(file=paste0(TravelDir,"/glp_drive_model_",as.character(region_id),".RData"))  
  drive_model<-z  
  rm(z)
  load(file=paste0(TravelDir,"/glp_transit_model_",as.character(region_id),".RData"))  
  trans_model<-zz
  rm(zz)
  
  colnames(orig_dest) <- c("origin_x","origin_y","dest_x","dest_y")

  traveltime<-orig_dest
  icomplete<-complete.cases(orig_dest)

  # Predict drive time
  print(paste0("Predicting drive time to ",destname))
  assign("x",predict(drive_model,newdata=orig_dest))
  
  # Drop outliers
  x[x<0] <- NA
  if (length(x)>=10000) {
    x999<-quantile(x,.999,na.rm=TRUE)
    x[x>=x999]<-NA
  }
  traveltime$x<-NA
  traveltime$x[icomplete]<-x
  colnames(traveltime)[ncol(traveltime)]<-paste0("drive_",destname)

  print(paste0("Predicting transit time to ",destname))
    
  # Predict transit time
  assign("x",predict(trans_model,newdata=orig_dest))
  
  # Drop outliers
  x[x<0] <- NA
  if (length(x)>=10000) {
    x999<-quantile(x,.999,na.rm=TRUE)
    x[x>=x999]<-NA
  }
  traveltime$x<-NA
  traveltime$x[icomplete]<-x
  colnames(traveltime)[ncol(traveltime)]<-paste0("trans_",destname)

  return(traveltime)
}


