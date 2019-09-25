# Estimating "Model 1" using B-splines
# log(price_{it})=a_{t}+b'z_{it}+g(e_i,n_i)+u_{it}
# Author: MM, LN
# Last modified: 2017/12/29

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
} else if (host=="jake.local" | host=="vic.local") {
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
source(paste0(RootDir,"/code/model/B1LoadLibrary.R"))
source(paste0(RootDir,"/code/model/B2SetPath.R"))


# Parameters    
N1<-0        # N1   = size of sample 1
             # 0    = full sample
nregs<-11       # Number of regions
LondonFlag<-0   # 0 : Only Greater London
                # 1 : London + some adjacent counties  

regnames<-data.frame(region_id=seq(1:nregs), 
                     region_str=c("CaMKOx", "CornwallDevon", 
                                  "EastMid", "EastEng", 
                                  "London", "NE", 
                                  "NW", "SE",
                                  "SW", "WestMid", "YorkshireHumber"))


for (r in 2:nregs) {
  print(paste0("Starting region ",r))
region_id<-regnames$region_id[r]    
region_str<-regnames$region_str[r] 
dirs<-B2SetPath(RootDir,region_id)
setwd(dirs$currentdir)

# Drop data prior to StartDate
StartDate<-as.Date("2008-01-01")

# Load data
if (LondonFlag==1 && region_id==5) {
m1data<-read.csv(paste0(dirs$datadir,"/model1_",region_str,"plus.csv"))
} else {
m1data<-read.csv(paste0(dirs$datadir,"/m11_",region_str,".csv"))
}

date1<-as.character(m1data$transferdate)
date2<-as.Date(date1,format="%Y-%m-%d %H:%M")
i1<-is.na(date2)
date2[i1]<-as.Date(date1[i1],format="%Y-%m-%d")
i1<-is.na(date2)
date2[i1]<-as.Date(date1[i1],format="%d/%m/%Y")

i1<-grep("transferyear",colnames(m1data))
colnames(m1data)[i1]<-"transferyear_old"
m1data$transferyear     <- year(date2)
m1data$transferdate_str <- date1
m1data$transferdate     <- date2
rm(date1,date2,i1)

set.seed(534)
if (N1>0) {
  m1data<-m1data[sample(nrow(m1data), N1), ]
}
m1data<-m1data[,c(1:7,67,73:76)]
save(m1data,file=paste0(RootDir,"/data/KPMG/region",r,".RData"))
}
