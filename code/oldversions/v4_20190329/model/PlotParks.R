parkfile<-paste0(RootDir,"/data/nationalparks/National_Parks_England.kml")
i1<-ogrListLayers(parkfile)
parks<-readOGR(parkfile,layer=i1)
ggplot(data=parks,aes(x,y))+geom_polygon(aes(col=data$Name))
