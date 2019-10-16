# Create dataset of non-commercial usecode for all UK
# Author: MM, LN
# Last modified: 2018.11.28

datastub<-"nondom"  # "nondom" = non-domestic
nregs<-11
N1   <- 100000
regnames<-data.frame(region_id=seq(1:nregs), 
                     region_str=c("CaMKOx", "CornwallDevon", 
                                  "EastMid", "EastEng", 
                                  "London", "NE", "NW", 
                                  "SE", "SW", "WestMid", 
                                  "YorkshireHumber"))
for (r in 1:11) {
  region_id<-regnames$region_id[r]    
  region_str<-as.character(regnames$region_str[r]) 
  dirs<-B2SetPath(RootDir,CodeDir,DataRoot,region_id,datastub)
  
  #-----------------------------------------------------------------
  #   Load and clean data
  #-----------------------------------------------------------------
  # Load data
  m1data<-read.csv(paste0(dirs$datadir,"/",datastub,"_",region_str,".csv"))
  
  # Set (long,lat) = (pcd)
  if (datastub=="nondom") {
    m1data$longitude<-m1data$longitude_pcd
    m1data$latitude <-m1data$latitude_pcd
  }
  
  # drop"Isles of Scilly"
  idrop<- (as.character(m1data$laname)=="Isles of Scilly")
  m1data<-m1data[!idrop,]
    
  # nondom data only
  iNoMiss<-complete.cases(m1data$rateablevalue, m1data$latitude, m1data$longitude, m1data$totalarea)
  m1data<-m1data[iNoMiss,]
  
  # Drop observatations with rateablevalue==0
  #   These are vacant, under construction or otherwise have zero rateable value
  #   however, economic value is not zero
  izero<-m1data$rateablevalue>0
  m1data<-m1data[izero,]
    
  m1data$logprice<-log(m1data$rateablevalue)
  m1data$price        <- m1data$rateablevalue/1000
    
  i1<-grep("totalarea",colnames(m1data))
  colnames(m1data)[i1]<-"total_floor_area"
    
  u1 <- A1NewUseCode(m1data$usecode)
  m1data$usecode <- A1NewUseCode(m1data$usecode)
  
  # Extract subset of variables
  if (r==1) {
    useCodeData<-m1data[,c("usecode","total_floor_area","modelregion")]
  } else {
    useCodeData<- rbind(useCodeData,m1data[,c("usecode","total_floor_area","modelregion")]) 
  }
  save(useCodeData,file=paste0(dirs$rootdir,"/data/nondom/usecodedata.RData"))    
}

set.seed(12049)
useCodeData<-useCodeData[sample(nrow(useCodeData), N1), ]
save(useCodeData,file=paste0(dirs$rootdir,"/data/nondom/usecodedata.RData"))    

