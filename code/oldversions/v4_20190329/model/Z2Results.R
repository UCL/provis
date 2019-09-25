# Load mapfile

OutDir<-paste0(CodeDir,"/output/allregions")

projects1<-data.frame(region=rep(1,5),
                      type=c("settlement","road","rail","road","rail"),
                      mapfile=c(paste0(OutDir,"/kml/Oxford Greenbelt.kml"),
                                "NA","NA",
                                paste0(OutDir,"/kml/region1_speed1.kml")),
                      pid=c(89,85,86,87,88))
projects2<-data.frame(region=rep(3,5),
                      type=c("settlement","road","rail","road","rail"),
                      mapfile=c(paste0(OutDir,"/kml/region3_settlement1.kml"),
                                "NA","NA",
                                paste0(OutDir,"/kml/region3_speed1.kml")),
                      pid=c(90:94))
                                                               
for (i1 in 1:2) {
  layer      <- ogrListLayers(mapfile[[i1]])
  settlement <- readOGR(mapfile[[i1]],layer=layer)
  map1<-get_map(coordinates(gCentroid(settlement)))
  ggmap(map1)+geom_polygon(data=fortify(settlement),aes(long,lat),alpha=.3)
  dev.copy(postscript,file=paste0(OutDir,))
}

s1<-settlement
c0<-gCentroid(settlement)
c1<-geocode("Ockbrook, England")
for (i1 in 1:2) {
  s1@polygons[[1]]@Polygons[[1]]@coords[,i1]<- s1@polygons[[1]]@Polygons[[1]]@coords[,i1] +
                                                c1[,i1]-coordinates(c0)[i1]
}
map2<-get_map(coordinates(gCentroid(s1)))
ggmap(map1)+geom_polygon(data=fortify(s1),aes(long,lat),alpha=.3)
