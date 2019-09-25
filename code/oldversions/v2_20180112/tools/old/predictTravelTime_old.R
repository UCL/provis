#' predictTravelTime(newdata,TravelModelDir)
#' This function predicts travel times to various destinations for newdata
#' 
#' @param newdata         data on (lat,long):  newdata[,1:2] = [latitude,longitude]
#' @param TravelModelDir  dir. name = location of saved travel models
#' @return newdata        Predict travel times and append to newdata
#' @keywords travel time
#' @export
#' @examples
#' data1 <-predictTravelTime(newdata,TravelModelDir)
predictTravelTime<- function(newdata,TravelModelDir) {

# Load models to predict travel times to (OX,MK,CAM)
load(file=paste0(TravelModelDir,"/model_drive_OX.RData"))
load(file=paste0(TravelModelDir,"/model_drive_CAM.RData"))
load(file=paste0(TravelModelDir,"/model_drive_MK.RData"))

load(file=paste0(TravelModelDir,"/model_trans_EUS.RData"))
load(file=paste0(TravelModelDir,"/model_trans_KGX.RData"))
load(file=paste0(TravelModelDir,"/model_trans_LIV.RData"))
load(file=paste0(TravelModelDir,"/model_trans_MAR.RData"))
load(file=paste0(TravelModelDir,"/model_trans_PAD.RData"))

load(file=paste0(TravelModelDir,"/model_trans_OX.RData"))
load(file=paste0(TravelModelDir,"/model_trans_CAM.RData"))
load(file=paste0(TravelModelDir,"/model_trans_MK.RData"))

# Make predictions: travel times to (CAM,MK,OX)
names(newdata)[1:2]<-c("m1data.latitude","m1data.longitude")

#
timenames_in<-c("drive_OX","drive_MK","drive_CAM","trans_EUS",
                "trans_KGX","trans_LIV","trans_MAR","trans_PAD",
                "trans_OX","trans_MK","trans_CAM")
timenames_out<-c("drive_OX","drive_MK","drive_CAM","train_EUS",
                 "train_KGX","train_LIV","train_MAR","train_PAD",
                 "train_OX","train_MK","train_CAM")
i1<-0
n<-ncol(newdata)
for (j in timenames_in) {
  i1<-i1+1
  n<-n+1
  print(paste0("Predicting ",timenames_out[i1],": ",
               as.character(i1)," of ",as.character(length(timenames_in))))
  assign("x",predict(eval(as.name(paste0("model_",j))), newdata=newdata))
  newdata<-data.frame(newdata,x)
  names(newdata)[n]<-timenames_out[i1]
}
names(newdata)[1:2]<-c("latitude","longitude")
return(newdata)
}


