MapDir<-paste0(RootDir,"/data/maps")
coastshapefile<-"ne_10m_coastline"

coast<-readOGR(MapDir,layer=coastshapefile)

# bounding box
wgs.84    <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
ukbbx <- readWKT("POLYGON((-7.5600 49.9600, 1.7800 49.9600, 1.7800 60.8400, -7.5600 60.8400, -7.5600 49.9600))",
               p4s=CRS(wgs.84))

englandcoast<-gIntersection(coast,ukbbx)

# 385  = england
# 1239 = Isle of Wight
iuk<-gIntersects(coast,ukbbx,byid=TRUE)
englandcoast<-coast[c(385,1239),]

writeOGR(englandcoast,dsn=MapDir,layer="englandcoast",driver="ESRI Shapefile")
