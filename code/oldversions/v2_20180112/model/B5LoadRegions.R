

B5LoadRegions<-function(dirs) {
  
  # Name of region file and road file
  regionshapefile<-"Regions_December_2015_Generalised_Clipped_Boundaries_in_England"
  wgs.84    <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  
  # Load region shape file
  regions<-readOGR(dirs$mapdir,layer=regionshapefile)
  regions<-spTransform(regions,CRS(wgs.84))
}

# Plot the regions in color with a legend
#regions<-B5LoadRegions(dirs)
#nregions<-nrow(regions@data)
#plot(regions, col=rainbow(nregions),main="UK regions with data availability")
#legend("left",legend = levels(regions@data$rgn15nm),
#       fill=rainbow(nregions),
#       cex=0.8)


