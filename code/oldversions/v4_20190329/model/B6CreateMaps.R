# TODO: 
#   1) create shape file of "adjusted" NIC regions
#   2) create map of each region alone
#   3) 

#source(paste0(RootDir,"/code/model/B2SetPath.R"))
source(paste0(CodeDir,"/model/B5LoadRegions.R"))

nregs<-11
regnames<-data.frame(region_id=seq(1:nregs), 
                     region_str=c("CaMKOx", "CornwallDevon", 
                                  "EastMid", "EastEng", 
                                  "London", "NE", 
                                  "NW", "SE",
                                  "SW", "WestMid", "YorkshireHumber"))
region_bbx<-vector("list",11)

# Create bounding box for spatial data
wgs.84    <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
uk_bbx <- readWKT("POLYGON((-7.5600 49.9600, 1.7800 49.9600, 1.7800 60.8400, -7.5600 60.8400, -7.5600 49.9600))",
                  p4s=CRS(wgs.84))


# MAP 1:  Plot the regions in color with a legend
region_id<-0
dirs<-B2SetPath(RootDir,CodeDir,region_id)

regions<-B5LoadRegions(dirs)
nregions<-nrow(regions@data)
#plot(regions, col=rainbow(nregions),main="UK regions with data availability")
#legend("left",legend = as.character(regions@data$rgn15nm),
#       fill=rainbow(nregions),cex=0.5,xjust=0)
#dev.copy2pdf(file=paste0(dirs$wwwdir,"/regions.pdf"))

png(file=paste0(dirs$wwwdir,"/regions.png"),
    width = 800,height = 600)
plot(regions, col=rainbow(nregions),main="UK regions with data availability")
legend("left",legend = as.character(regions@data$rgn15nm),
       fill=rainbow(nregions),cex=1,xjust=0.2)
dev.off()

# TO DO:  make the legend smaller

# plot each region separately
nregs<-11
regnames$ONS <- c(6,9,4,6,7,1,2,8,9,5,3)
                  
for (r in 1:nregs) {
  dirs<-B2SetPath(RootDir,CodeDir,r)
  DestinationFile<-paste0(dirs$datadir,"/destinations",r,".csv")
  dest<-read.csv(DestinationFile)  
  dest<-dest[dest$shortlist,]
  title=paste0("Region: ",regnames[r,2])
  
  png(file=paste0(dirs$wwwdir,"/region",as.character(r),".png"),
      width = 800,height = 600)
  
  plotstyle <- 2
  ndest<-nrow(dest)
  destname<-vector('character',ndest)
  for (i1 in 1:ndest) {
    destname[i1] <- strsplit(as.character(dest$name[i1]),",")[[1]][1]
  }
  if (r !=1) {
    if (plotstyle==1) {
      plot(regions[regnames$ONS[r],],main=title)
      points(dest[,c("longitude","latitude")],col="red",pch=19)
      text(dest[,c("longitude","latitude")],pos=1,labels=dest[,"shortname"])
    } else if (plotstyle==2) {
      plot(regions[regnames$ONS[r],],main=title)
      points(dest[,c("longitude","latitude")],col=rainbow(nrow(dest)),pch=19)
      legend("left",legend = destname,
             fill=rainbow(nrow(dest)),
             cex=0.8)
    }
  } else {
    # CAMKOX = union(4,8)
    i1 <- (levels(regions@data$rgn15nm)[regions@data$rgn15nm]=="East of England" |
             levels(regions@data$rgn15nm)[regions@data$rgn15nm]=="South East")   
    if (plotstyle==1) {
      plot(regions[i1,],main=title)
      points(dest[,c("longitude","latitude")],col="red",pch=19)
      text(dest[,c("longitude","latitude")],pos=1,labels=dest[,"shortname"])
    } else if (plotstyle==2) {
      plot(regions[i1,],main=title)
      points(dest[,c("longitude","latitude")],col=rainbow(nrow(dest)),pch=19)
      legend("left",legend = destname,
             fill=rainbow(nrow(dest)),
             cex=0.8)
    }
  }
  dev.off()
#  dev.copy2pdf(file=paste0(dirs$wwwdir,"/region",as.character(r),".pdf"))
}


