DistanceToRoad<-function(houses,roadtype="motorways",region_bbx,dirs) {

#  layer1<-ogrListLayers(paste0(dirs$roaddir,"/motorways.kml"))
#  motorways<-readOGR(dsn=paste0(dirs$roaddir,"/motorways.kml"),layer=layer1)
  # roadtype = "motorways" or "aroads"
   roads0<-readOGR(dsn=dirs$roaddir,layer=roadtype)
  
  ncores<-detectCores()-1
  cl<-makeCluster(ncores,type="FORK")
  
  roads<-gIntersection(region_bbx,roads0)
  
  n<-nrow(houses)
  n1<-floor(n/ncores)
  rem<-n-n1*ncores
  n2<-n1+1
  i1<-seq(1,n2*rem,by=n2)
  i2<-i1+n2-1
  i3<-max(i2)+seq(1,n1*(ncores-rem),by=n1)
  i4<-i3+n1-1
  i1<-c(i1,i3)
  i2<-c(i2,i4)

  houses1<-vector('list',length=ncores)
  for (j1 in 1:ncores) {
    houses1[[j1]]<-houses[i1[j1]:i2[j1],]  
  }

  clusterExport(cl,"roads",envir=environment())
  distance<-parLapply(cl,houses1,function(x) dist2Line(x,roads))
  
  distance1<-distance[[1]]
  for (i1 in 2:ncores) {
    distance1<-rbind(distance1,distance[[i1]])
  }
  
  on.exit(stopCluster(cl))
  distance1<-matrix(NA,n,4)
  for (j1 in 1:ncores) {
    distance1[i1[j1]:i2[j1],]<-distance[[j1]]  
  }
  colnames(distance1)<-colnames(distance[[1]])
  return(distance1)
}

#getDist<-function(i1,i2,x,lines1) {
#  y<-dist2Line(x[i1:i2,],lines1)
#  return(y)
#}
