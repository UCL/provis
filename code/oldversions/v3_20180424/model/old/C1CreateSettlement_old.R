# Create default settlement for each region

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

nregs<-11       # Number of regions
LondonFlag<-1   # 0 : Only Greater London
# 1 : London + some adjacent counties  

regnames<-data.frame(region_id=seq(1:nregs), 
                     region_str=c("CaMKOx", "CornwallDevon", 
                                  "EastMid", "EastEng", 
                                  "London", "NE", 
                                  "NW", "SE",
                                  "SW", "WestMid", "YorkshireHumber"))
# newlocation:  names of default new settlement locations
#               1 name for each region
#             Region     New Location     Description   Model town
#             CaMKOx         Bedford                     Bedford
#             CornwallDevon  Nadderwater  near Exeter    Exeter  
#             EastMid
modeltown<-c("Bedford, UK",
             "Exeter, UK",
             "Leicester, UK")
newtown<-c("bedford_new",
           "exeter_new",
           "gotham_new")
newlocation<-c("Bedford, UK",
               "Nadderwater, UK",
                "Gotham, UK",
                "4",
                "5",
                "6",
                "7",
                "8",
                "9",
                "10",
                "11")
region_bbx<-vector("list",11)

# Create bounding box for spatial data
wgs.84    <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
ukgrid    <-"+init=epsg:27700"
uk_bbx <- readWKT("POLYGON((-7.5600 49.9600, 1.7800 49.9600, 1.7800 60.8400, -7.5600 60.8400, -7.5600 49.9600))",
                  p4s=CRS(wgs.84))

for (r in 1:nregs) {
  region_id<-regnames$region_id[r]    
  region_str<-regnames$region_str[r] 
  source(paste0(RootDir,"/code/model/B2SetPath.R"))
  setwd(CurrentDir)
  
  # Load data
  settlement<-read.csv(paste0(RootDir,"/data/region1/settlements/bedford.csv"))
  
  # create spatial points object
  houses<-SpatialPoints(settlement[,c("longitude","latitude")],CRS(wgs.84))
  # convert to (easting,northing) and compute centroid
  houses_proj1<-spTransform(houses,CRS(ukgrid))
  h0<-gCentroid(houses_proj1)
  
  # find new centroid at Nadderwater near exeter
  h1<-SpatialPoints(geocode(newlocation[region_id],source="google"),CRS(wgs.84))
  h1<-spTransform(h1,CRS(ukgrid))
  
  # translate houses to the new location
  houses_proj2<-houses_proj1
  houses_proj2@coords[,1]<-coordinates(houses_proj1)[,1]-coordinates(h0)[,1]+coordinates(h1)[,1]
  houses_proj2@coords[,2]<-coordinates(houses_proj1)[,2]-coordinates(h0)[,2]+coordinates(h1)[,2]
  #  convert to (longitude,latitude)
  houses2<-spTransform(houses_proj2,CRS(wgs.84))
  newsettlement<-settlement
  newsettlement[,c("longitude","latitude")]<-coordinates(houses2)
  write.csv(newsettlement,file=paste0(NewDataDir,"/",newtown[region_id],".csv"),row.names=FALSE)
}