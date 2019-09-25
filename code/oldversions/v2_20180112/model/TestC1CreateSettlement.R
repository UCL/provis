# New settlement template
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
CodeDir<-paste0(RootDir,"/code/oldversions/v2_20180112")
source(paste0(CodeDir,"/model/B1LoadLibrary.R"))
source(paste0(CodeDir,"/model/B2SetPath.R"))
source(paste0(CodeDir,"/model/A4CreateSettlement.R"))
source(paste0(CodeDir,"/model/C1CreateSettlement.R"))

nregs<-11
regnames<-data.frame(region_id=seq(1:nregs), 
                     region_str=c("CaMKOx", "CornwallDevon", 
                                  "EastMid", "EastEng", 
                                  "London", "NE", "NW", 
                                  "SE", "SW", "WestMid", 
                                  "YorkshireHumber"))

r            <- 2
nhouses      <- 1000
popdensity   <- 35
model_laname <- "Exeter"
mapfile      <- paste0(dirs$newdatadir,"/East Midlands Settlement 1.kml")  
if (!file.exists(mapfile)) {
  print("kml file for new settlement does not exist. You must first create a kml file in the region.")  
  stop("Create kml file and then try to run program.")
}

# Set (region_id,region_str) and paths
region_id  <- regnames[r,1]
region_str <- regnames[r,2]
dirs       <- B2SetPath(RootDir,CodeDir,region_id)

# load data and list feasible lanames
load(paste0(dirs$datadir,"/m2data2.RData"))
feasible_lanames <- levels(m2data$lad15nm)

# Create settlement  
newdata<-C1CreateSettlement(nhouses,popdensity,mapfile,model_laname,m2data)
newdata2<-A4CreateSettlement(region_id,dirs,m2data_new = newdata) 

# TODO
#   1) check popdensity is in realistic range
#   2) check that map is in region
#   3) check that map file is kml file defining polygon
#   4) check that (popdensity,nhouses,area)  are consistent
#   5) check that laname is a valid laname in region
#   5) add options to vary or set other features of settlement
#   6) timing of steps