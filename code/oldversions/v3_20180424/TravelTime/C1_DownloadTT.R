# Travel time downloads for different regions
# Author: MM, LN
# Last modified: 2017/12/27

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
} else if (host=="jake.local" | host=="vic.local" | host=="zeppo-22-1.local") {
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

CurrentDir<-paste0(RootDir,"/code/currentversion/TravelTime")
DataDir<-paste0(RootDir,"/data")
TravelModelDir<-paste0(RootDir,"/currentversion/code/TravelTime")
OutDir <-paste0(DataDir,"/TravelTime")

setwd(CurrentDir)

#-----------------------------------------------------------------
# Install packages
#-----------------------------------------------------------------
#devtools::install_github("dkahle/ggmap")
library(ggmap)

#ggmap_credentials()
#register_google(key = "", account_type = "premium", day_limit = 100000)

# Set sample size
nPairs<-10000

set.seed(534)

nregs<-11
regnames<-data.frame(region_id=seq(1:nregs), 
                     region_str=c("CaMKOx", "CornwallDevon", 
                                  "EastMid", "EastEng", 
                                  "London", "NE", 
                                  "NW", "SE",
                                  "SW", "WestMid", "YorkshireHumber"))

for (r in 1:1) {
  region_id<-regnames$region_id[r]    
  region_str<-regnames$region_str[r] 
  dirs<-B2SetPath(RootDir,CodeDir,region_id)

  m1data<-read.csv(paste0(dirs$datadir,"/m11_",region_str,".csv"))
  m1data<-m1data[complete.cases(m1data[,c("longitude","latitude")]),]

  origins<-sample(c(1:nrow(m1data)),2*nPairs,replace=TRUE)
  samplename<-paste0("ttsample_",region_id)
  assign(samplename,data.frame(origin_x=m1data$longitude[origins[1:nPairs]],
                               origin_y=m1data$latitude[origins[1:nPairs]],
                               dest_x=m1data$longitude[origins[(nPairs+1):length(origins)]],
                               dest_y=m1data$latitude[origins[(nPairs+1):length(origins)]]))
}

#-----------------------------------------------------------------
# Geocode
#-----------------------------------------------------------------
snames<-ls(pattern = "ttsample")

# First 3 regions
#snames<-snames[1]

origin_address<-data.frame(origin_address=rep(NA,length.out=nPairs))
dest_address<-data.frame(dest_address=rep(NA,length.out=nPairs))

for (s in snames) {
  sn<-eval(as.name(s))
    for (i in 1:nPairs) {
      # Get address for each (lat,long)
      print(paste0("Address ",as.character(i)," of ",as.character(nPairs)))
      for (i2 in 1:20) {
        temp<-try(revgeocode(as.numeric(sn[i,1:2]),
                            override_limit=TRUE,
                            messaging=FALSE))
        errflag<- !grepl("[Ee]rror",temp)
        if (errflag) {
          origin_address[i,1]<-temp 
          break
        }
      }
      for (i2 in 1:20) {
        temp<-try(revgeocode(as.numeric(sn[i,3:4]),
                                          override_limit=TRUE,
                                          messaging=FALSE))
        errflag<- !grepl("[Ee]rror",temp)
        if (errflag) {
          dest_address[i,1]<-temp
          break
        }
      }
    }
  assign(s,data.frame(sn,origin_address,dest_address))
  origin_address<-data.frame(origin_address=rep(NA,length.out=nPairs))
  dest_address<-data.frame(dest_address=rep(NA,length.out=nPairs))
}

#-----------------------------------------------------------------
# Download travel times
#-----------------------------------------------------------------
for (s in snames) {

    currentsample<-eval(as.name(s))
    attach(currentsample)
    drive_times<-list()
    transit_times<-list()
    # START HERE
    for (q in 1:nPairs) {
          print(paste0("Route ",as.character(q)," of ",as.character(nPairs)))  
          for (i2 in 1:20) {
            temp<-  as.list(try(route(as.character(currentsample$origin_address[q]),
                                      as.character(currentsample$dest_address[q]), 
                                      mode="driving", structure = "route", 
                                      output = "simple", override_limit = TRUE,
                                      messaging=FALSE)))
            errflag<- !grepl("[Ee]rror",temp)
            if (errflag) {
              drive_t<-temp
              break
            }
          }
          drive_times<-append(drive_times,list(drive_t))
          for (i2 in 1:20) {
            temp<-as.list(try(route(as.character(currentsample$origin_address[q]), 
                                    as.character(currentsample$dest_address[q]),
                                    mode="transit", structure = "route", 
                                    output = "simple", override_limit = TRUE,
                                    messaging=FALSE)))
            errflag<- !grepl("[Ee]rror",temp)
            if (errflag) {
              transit_t<-temp
              break
            }
          }
          transit_times<-append(transit_times,list(transit_t))
    }

    drive_times_m<-matrix(nrow=length(drive_times), ncol=1)
    lng<-length(drive_times_m)
    
        for (q in 1:lng) {
          drive_times_m[q]<-sum(drive_times[[q]]$minutes,na.rm=TRUE)
        }
    
    is.na(drive_times_m) <- !drive_times_m
    
    transit_times_m<-matrix(nrow=length(transit_times), ncol=1)
    lng<-length(transit_times_m)
    
        for (q in 1:lng) {
          transit_times_m[q]<-sum(transit_times[[q]]$minutes,na.rm=TRUE)
        }
    
    is.na(transit_times_m) <- !transit_times_m
    
    # Find indices with missing data
    miss_ind<-which(is.na(drive_times_m))
    
    drive_times_miss<-list()
    
        for (p in miss_ind) {
          drive_t <- as.list(try(route(as.character(currentsample$origin_address[p]),
                                       as.character(currentsample$dest_address[p]), 
                                       mode="driving", structure = "route",
                                       output = "simple", override_limit = TRUE,
                                       messaging=FALSE)))
          drive_times_miss<-append(drive_times_miss,list(drive_t))
        }
    
    drive_times_miss_m<-matrix(nrow=length(drive_times_miss), ncol=1)
    lng<-length(drive_times_miss_m)
    
     if (lng>0){
        for (q in 1:lng) {
          drive_times_miss_m[q]<-sum(drive_times_miss[[q]]$minutes,na.rm=TRUE)
        }}
    
    is.na(drive_times_miss_m) <- !drive_times_miss_m
    drive_times_miss_m2<-data.frame(miss_ind,drive_times_miss_m)
    miss_ind2<-which(is.na(drive_times_miss_m))
    
    drive_times_m2<-data.frame(1:nPairs,drive_times_m)
    names(drive_times_m2)<-c("ind","time")
    names(drive_times_miss_m2)<-c("ind","time")
    
    drive_times_m2$time[is.na(drive_times_m2$time)] <- drive_times_miss_m2$time
    
    # Find indices with missing data
    miss_ind<-which(is.na(transit_times_m))
    
    transit_times_miss<-list()
        for (p in miss_ind) {
          for (i2 in 1:20) {
            temp<-as.list(try(route(as.character(currentsample$origin_address[p]), 
                                    as.character(currentsample$dest_address[p]), 
                                    mode="transit", structure = "route", 
                                    output = "simple", override_limit = TRUE,
                                    messaging=FALSE)))
            if (!is.na(temp[1])) {
              transit_t<-temp
              break
            }
          }
          transit_times_miss<-append(transit_times_miss,list(transit_t))
        }
    
    transit_times_miss_m<-matrix(nrow=length(transit_times_miss), ncol=1)
    lng<-length(transit_times_miss_m)
    
    if (lng>0) {
        for (q in 1:lng) {
          transit_times_miss_m[q]<-sum(transit_times_miss[[q]]$minutes,na.rm=TRUE)
        }}
    
    is.na(transit_times_miss_m) <- !transit_times_miss_m
    transit_times_miss_m2<-data.frame(miss_ind,transit_times_miss_m)
    miss_ind2<-which(is.na(transit_times_miss_m))
    
    transit_times_m2<-data.frame(1:nPairs,transit_times_m)
    names(transit_times_m2)<-c("ind","time")
    names(transit_times_miss_m2)<-c("ind","time")
    
    transit_times_m2$time[is.na(transit_times_m2$time)] <- transit_times_miss_m2$time
    
assign(s,data.frame(currentsample,drive_times_m2$time,transit_times_m2$time))

}
snames2<-ls(pattern="ttsample")

for (z in snames) {
  zz<-get(z)
  colnames(zz)[7:8]<-c("drive_time", "transit_time")  
  assign(z,zz)
  write.csv(eval(as.name(z)), file=paste0(OutDir,"/00_",z,".csv"))
}

