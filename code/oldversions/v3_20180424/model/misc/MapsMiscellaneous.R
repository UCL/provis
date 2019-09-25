
# read shape files
# TODO: create shape file of NIC regions

RootDir<-"/Users/larsnesheim/Documents/research/hedonic/NIC"
MapDir<-paste0(RootDir,"/data/maps")
OutDir<-paste0(RootDir,"/code/output")
setwd(MapDir)
wgs.84    <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'


# Name of region file and road file
regionshapefile<-"Regions_December_2015_Generalised_Clipped_Boundaries_in_England"
routefile<-paste0(MapDir,"/road1.kml")

# Load region shape file
regions<-readOGR(MapDir,layer=regionshapefile)
regions<-spTransform(regions,CRS(wgs.84))
region_str<-levels(regions$rgn15nm)

# Load road file
layer<-ogrListLayers(routefile)
route0<-readOGR(routefile,layer=layer)
route<-as.data.frame(route0)[,c(1,3,4)]

# Pick region
region_id<-2

# plot regions with current region highlighted
plot(regions)
plot(regions[regions$objectid==region_id,],col="red",add=TRUE)
title(main="England")
dev.copy2eps(file=paste0(OutDir,"/regions.eps"))
dev.copy2pdf(file=paste0(OutDir,"/regions.pdf"))

dev.off()


# Plot route
plot(regions[regions$objectid==region_id,])
lines(route[,2:3],col="black")
dev.copy2eps(file=paste0(OutDir,"/route1.eps"))
dev.copy2pdf(file=paste0(OutDir,"/route1.pdf"))

# Plot the regions in color with a legend
source(paste0(RootDir,"/code/model/B5LoadRegions.R"))
if (!exists("dirs")) {
  region_id<-0
  source(paste0(RootDir,"/code/model/B2SetPath.R"))
}
regions<-B5LoadRegions(dirs)
nregions<-nrow(regions@data)
plot(regions, col=rainbow(nregions),main="UK regions with data availability")
legend("left",legend = as.character(regions@data$rgn15nm),
       fill=rainbow(nregions),
       cex=0.8)
dev.copy2pdf(file=paste0(dirs$outdir,"/regions.pdf"))

# plot only current region