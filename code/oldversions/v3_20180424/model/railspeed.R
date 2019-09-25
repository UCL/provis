if (infrastructure_type=="road") {
  ispline<-grep("spline_drive",colnames(m2data))
  inew<-grep("\\<drive_",colnames(m2data))
} else if (infrastructure_type=="rail") {
  ispline<-grep("spline_trans",colnames(m2data))
  inew<-grep("\\<trans_",colnames(m2data))
}  

m2data<-m2data[,-(ispline)]

if (is.null(shapefile)) {
  # Change in travel time applies to whole region
  m2data[,inew] <- multiplier*m2data[,inew]
} else {
  # Change in travel time applies only to subset of region
  # that intersects with polygon defined in shapefile
  if (usemapdir) {
    shapefile<-paste0(dirs$mapdir,"/",shapefile)
  }  
  layer1<-ogrListLayers(shapefile)
  speedregion<-readOGR(shapefile,layer=layer1)
  i1<-gIntersects(SpatialPoints(m2data[,c("longitude","latitude")],CRS(wgs.84)),
                  speedregion,byid=TRUE)
  m2data[i1[1,],inew]<-multiplier*m2data[i1[1,],inew]
}
load(file=paste0(dirs$outdir,"/m2dataspline.RData"))

if (infrastructure_type=="road") {
  inew<-grep("drive_",m2dataspline$varlist)
} else if (infrastructure_type=="rail") {
  inew<-grep("trans_",m2dataspline$varlist)
}

for (i1 in inew) {
  m2data$tempB<-bSpline(m2data[,m2dataspline$varlist[i1]],
                        knots=m2dataspline$knots[[i1]],
                        degree=m2dataspline$degree[i1],intercept=FALSE)
  iTempB<-grep("tempB",colnames(m2data))
  names(m2data)[iTempB]<-paste0("spline_",m2dataspline$varlist[i1])
}
