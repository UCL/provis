# Model 2:  Create estimation sample

#-----------------------------------------------------------------
# Set working directory
# Define path names
#-----------------------------------------------------------------
host<-system("hostname",intern=TRUE)
if (host=="dh-230-mac.econ.ucl.ac.uk") {
  RootDir<-"/Users/uctpln0/Documents/research/hedonic/NIC"
} else if (host=="minelava") {
  RootDir<-"C:/a/research/hedonic/NIC"
} else if (host=="jake.local" | host=="vic.local") {
  RootDir<-"/home/uctpln0/hedonic/NIC"
} else if (host=="MateuszsMacBook") {
  RootDir<-"/Users/mateuszmysliwski/Dropbox/NICProject"
} else {
  info_sys<-Sys.info()
  user<-info_sys["user"]
  if (user=="uctpln0") {
    RootDir<-"/home/uctpln0/hedonic/NIC"
  } else {
    RootDir<-"/Users/larsnesheim/Documents/research/hedonic/NIC"
  }
}
source(paste0(RootDir,"/code/model/A0SetPath.R"))

#-----------------------------------------------------------------
#   Load and clean data
#-----------------------------------------------------------------

m2data<-read.csv(paste0(DataDir,"/m2data0.csv"),header=TRUE)

m2data<-m2data[,-1]

# Make predictions: travel times to (CAM,MK,OX)
ilat<-grep("latitude\\>",colnames(m2data))
ilon<-grep("longitude\\>",colnames(m2data))
newdata<-predictTravelTime(m2data[,c(ilat,ilon)],TravelModelDir)
m2data<-join(m2data,newdata,by=c("latitude","longitude"),match="first",type="left")  

# Check that travel predictions make sense
#  distGeo(c(long1,lat1),c(long2,lat2))  compute distance from 1 to 2
DestinationFile<-paste0(DataDir,"/destinations.csv")
if (!file.exists(DestinationFile)) {
  dest_name<-c("Bedford, England", 
               "Cambridge, England",
               "Milton Keynes, England",
               "Oxford, England",
               "Luton Airport, Luton",
               "Stansted Airport, Stansted",
               "Liverpool Street Station, Liverpool Street",
               "St Pancras Station, Euston Road",
               "Marylebone Station, London",
               "Euston Station, London",
               "London King's Cross, Euston Road")
  dest_name_short<- c("BED","CAM","MK","OX","LUT",
                      "STD","LIV","STP","MAR","EUS","KGX")

  dest_coordinates<- geocode(dest_name,output="latlon",source="google")  
  if (any(is.na(dest_coordinates))) {
    for (i1 in 1:11) {  
      if (is.na(dest_coordinates[i1,1])) {
        dest_coordinates[i1,]<- geocode(dest_name[i1],output="latlon",source="google")  
      }   
    }  
  }
  names(dest_coordinates)<-c("longitude","latitude")
  dest<-data.frame(dest_name,dest_name_short,dest_coordinates)
  write.csv(dest,DestinationFile)
} else {
  dest<-read.csv(DestinationFile)  
}

n<-ncol(m2data)
ilat<-grep("latitude",colnames(m2data))
ilon<-grep("longitude",colnames(m2data))
for (i1 in 1:11) {
  n<-n+1
  assign("x",distGeo(m2data[,c(ilon,ilat)],dest[i1,c("longitude","latitude")])/1000)
  m2data$tempx<-x
  names(m2data)[n]<-paste0("distance_",dest$dest_name_short[i1])
}
rm(dest_name)
rm(dest_name_short)
rm(dest_coordinates)
rm(i1,n,x)

m2data$train_LON <- with(m2data,
                         pmin(m2data$train_EUS,m2data$train_KGX,
                              m2data$train_PAD,m2data$train_LIV,
                              m2data$train_MAR))
rm(newdata)
rm(ilat,ilon,host)

# Replace non-positive train times with missing values
m2data$train_CAM[m2data$train_CAM<=0]<-NA
m2data$train_MK[m2data$train_MK<=0]<-NA
m2data$train_OX[m2data$train_OX<=0]<-NA

# Create splines
m2spline<-list(c("drive_CAM","train_CAM","drive_OX","train_OX","train_LON"))
names(m2spline)[1]<-"varlist"
m2spline$degree<-rep(3,5)
m2spline$segments <- rep(4,5)
for (i1 in 1:5) {
  i2<-grep(m2spline$varlist[i1],colnames(m2data))
  tempknots<-quantile(m2data[,m2spline$varlist[i1]],seq(0.1,0.9,length.out=m2spline$segments[i1]))
  if (i1==1) {
    m2spline$knots<-list(tempknots)
  } else {
    m2spline$knots<-list(m2spline$knots,tempknots)
  }
  names(m2spline$knots)["tempknots"]<-m2spline$varlist[i1]
  
  m2data$tempB<-bSpline(m2data[,m2spline$varlist[i1]],
                        knots=tempknots,
                        degree=m2spline$degree[i1],intercept=FALSE)
  iTempB<-grep("tempB",colnames(m2data))
  names(m2data)[iTempB]<-paste0("spline_",m2spline$varlist[i1])
}
#-----------------------------------------------------------------
#  Save data
#-----------------------------------------------------------------

attach(m2data)
write.csv(m2data,paste0(DataDir,"/m2data1.csv"))
save(m2data,file=paste0(DataDir,"/m2data.RData"))
