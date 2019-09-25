

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

RoadDir<-paste0(RootDir,"/data/roads/oproad_essh_gb/data")
layer1<-"HP_RoadLink"
layer2<-"HP_RoadNode"
layer3<-"SD_MotorwayJunction"

hp1<-readOGR(dsn=RoadDir,layer=layer1)
hp2<-readOGR(dsn=RoadDir,layer=layer2)
sd3<-readOGR(dsn=RoadDir,layer=layer3)
