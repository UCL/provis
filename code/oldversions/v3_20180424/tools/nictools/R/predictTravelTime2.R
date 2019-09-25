#' predictTravelTime2(orig_dest,region_id,TravelDir)
#' This function predicts travel times from orgin to dest, for region_id
#'
#' @param orig_dest       data on (longitude,latitude) for (origin,destination) pairs
#' @param destname       short name of destination
#' @param region_id       id number for region between 1 and 11
#' @param TravelDir       directory containing travel model results
#' @param printstr        name of destination to print to std. io.
#' @param basis           c("glp","tensor","cheb"): corresponding travelmodel results must exist
#' @return traveltime     Add predicted travel times to origin
#' @keywords travel time
#' @export
#' @examples
#' printstr<-"default"
#' traveltime <-predictTravelTime2(orig_dest,destname,region_id,TravelDir,printstr,"glp")
#' printstr<-"Dartmoor"
#' traveltime <-predictTravelTime2(orig_dest,destname,region_id,TravelDir,printstr,"glp")
#' traveltime <-predictTravelTime2(orig_dest,destname,region_id,TravelDir,printstr,"cheb")
predictTravelTime2<- function(orig_dest,destname,region_id,TravelDir,printstr="default",
                              basis="glp") {

  if (printstr=="default") {
    printstr<-destname
  }
  # Load travel prediction models
  # load drivemodel and transmodel
  load(file=paste0(TravelDir,"/",basis,"_drive_model_",as.character(region_id),".RData"))
  load(file=paste0(TravelDir,"/",basis,"_transit_model_",as.character(region_id),".RData"))

  colnames(orig_dest) <- c("origin_x","origin_y","dest_x","dest_y")

  traveltime<-orig_dest
  icomplete<-complete.cases(orig_dest)

  # Predict drive time
  print(paste0("Predicting drive time to ",printstr))
  if (basis=="glp" | basis=="tensor") {
    x<-predict(drivemodel,newdata=orig_dest)
  } else if (basis=="cheb") {
    if (drivemodel$distflag==0) {
      # basis = tensor product ofchebyshev polynomials
      xx<-tensorCheb(orig_dest,drivemodel$k,drivemodel$lo,drivemodel$hi)
    } else if (drivemodel$distflag==1) {
      orig_dest$distance <- sqrt( (orig_dest$origin_x-orig_dest$dest_x)^2
                                 +(orig_dest$origin_y-orig_dest$dest_y)^2)
      orig_dest$theta    <- acos((orig_dest$dest_x-orig_dest$origin_x)/orig_dest$distance)
      xx<-tensorCheb(orig_dest[,c("origin_x","origin_y","distance","theta")],drivemodel$k,
                     drivemodel$lo,drivemodel$hi)
    }
    x<- as.matrix(xx$x) %*% as.matrix(drivemodel$b)
    if (drivemodel$logflag==1) {
      x<-exp(x)
    } else if (drivemodel$logflag==2) {
      x<- drivemodel$drivelo + (drivemodel$drivehi-drivemodel$drivelo) *
          exp(x)/(1+exp(x))
    }
  }

  # Drop outliers
  x[x<0] <- NA
  if (length(x)>=10000) {
    x999<-quantile(x,.999,na.rm=TRUE)
    x[x>=x999]<-NA
  }
  traveltime$x<-NA
  traveltime$x[icomplete]<-x
  colnames(traveltime)[ncol(traveltime)]<-paste0("drive_",destname)

  print(paste0("Predicting transit time to ",printstr))

  # Predict transit time
  if (basis=="glp" | basis=="tensor") {
      x<-predict(trans_model,newdata=orig_dest)
  } else if (basis=="cheb") {
    if (transmodel$distflag==0) {
      xx<-tensorCheb(orig_dest,transmodel$k,transmodel$lo,transmodel$hi)
    } else if (transmodel$distflag==1) {
      orig_dest$distance <- sqrt( (orig_dest$origin_x-orig_dest$dest_x)^2
                                  +(orig_dest$origin_y-orig_dest$dest_y)^2)
      orig_dest$theta    <- acos((orig_dest$dest_x-orig_dest$origin_x)/orig_dest$distance)
      xx<-tensorCheb(orig_dest[,c("origin_x","origin_y","distance","theta")],transmodel$k,
                     transmodel$lo,transmodel$hi)
    }
    x<-as.matrix(xx$x) %*% as.matrix(transmodel$b)
    if (transmodel$logflag==1) {
      x<-exp(x)
    } else if (transmodel$logflag==2) {
      x<- transmodel$translo + (transmodel$transhi-transmodel$translo) *
          exp(x)/(1+exp(x))
    }
  }
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


