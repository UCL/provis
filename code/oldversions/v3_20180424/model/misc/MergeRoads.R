library(rgeos)
library(rgdal)
library(ggmap)

RootDir<-"/Users/larsnesheim/Documents/research/hedonic/NIC"
roaddir_raw<-paste0(RootDir,"/data/roads_shp/oproad_essh_gb/data")
roaddir<-paste0(RootDir,"/data/roads")

wgs.84    <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

files<-dir(roaddir_raw)

ishape<-grep("RoadLink\\.shp\\>",files)

#shortname<-strsplit(files,"_")
#n<-length(shortname)
#shortname<-t(matrix(unlist(shortname),2,n))

#roadtype<-shortname[,2]
#shortname<-unique(shortname[,1])

#roadtype<-strsplit(roadtype,"\\.")
#roadtype<-unique(t(matrix(unlist(roadtype),2,n))[,1])

browser()
motorways<-NULL
aroads<-NULL
i3<-0
for (i2 in ishape) {
  i3<-i3+1
  #i2<-ishape[81]
  layer1<-gsub("\\.shp","",files[i2])
  road<-readOGR(dsn=roaddir_raw,layer=layer1)
  
  motorways_temp<-road[as.character(road@data$class)=="Motorway",]
  aroads_temp   <-road[as.character(road@data$class)=="A Road",]
  
  if (is.null(motorways) & nrow(motorways_temp)>0) {
    motorways_temp<-spTransform(motorways_temp,CRS(wgs.84))
    motorways<-motorways_temp
  } else if (!is.null(motorways) & nrow(motorways_temp)>0) {
    motorways_temp<-spTransform(motorways_temp,CRS(wgs.84))
    motorways<-rbind(motorways,motorways_temp)  
  }
  if (is.null(aroads) & nrow(aroads_temp)>0) {
    aroads_temp<-spTransform(aroads_temp,CRS(wgs.84))
    aroads   <-aroads_temp
  } else if (!is.null(aroads) & nrow(aroads_temp)>0) {
    aroads_temp<-spTransform(aroads_temp,CRS(wgs.84))
    aroads   <-rbind(aroads,aroads_temp)
  }  
  print(paste0("Merged ",i3," out of ",length(ishape)," shape files."))
  if (!is.null(motorways)) {
#    writeOGR(motorways,paste0(roaddir,"/motorways.kml"),layer="motorway","KML",overwrite_layer=TRUE)
    writeOGR(motorways,dsn=roaddir,layer="motorways",driver="ESRI Shapefile",overwrite_layer=TRUE)
  }
  if (!is.null(aroads)) {
#    writeOGR(aroads,paste0(roaddir,"/aroads.kml"),layer="aroad","KML",overwrite_layer=TRUE)
    writeOGR(aroads,dsn=roaddir,layer="aroads",driver="ESRI Shapefile",overwrite_layer=TRUE)
  }
}