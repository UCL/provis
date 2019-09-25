
source("A0Setup.R")
nregs<-11
regnames<-data.frame(region_id=seq(1:nregs), 
                     region_str=c("CaMKOx", "CornwallDevon", 
                                  "EastMid", "EastEng", 
                                  "London", "NE", "NW", 
                                  "SE", "SW", "WestMid", 
                                  "YorkshireHumber"),
                     name=c("Cambridge, Milton Keynes, and Oxford",
                            "Cornwall and Devon",
                            "East Midlands",
                            "East of England",
                            "London","North East",
                            "North West","South East",
                            "South West","West Midlands",
                            "Yorkshire and the Humber"))
region_map_index<-c(3,9,4,6,7,1,2,8,9,5,3)
project_id          <- 3
infrastructure_type <- "settlement"   # c("rail","road","settlement")
                
# parameters of new settlement
load(file=paste0("www/pid",project_id,"/parms.RData"))

nhouses      <- parms$nhouses
popdensity   <- parms$popdensity
model_laname <- parms$la_model
#mapfile      <- parms$mapfile
mapfile      <-paste0(CodeDir,"/shiny/kml/region2settlement1.kml")
region_id    <- parms$region_id
dirs         <-B2SetPath(RootDir,CodeDir,region_id)

price_per_acre <- c(4000,7000,10000)
sqm_per_acre <- 4046.86   # square meters per acre
price_per_sqm <- price_per_acre/sqm_per_acre
m2flag        <- 1  # 0 = m2ols0, 1 = m2ols1

# END OF PARAMETERS
region_str      <- as.character(regnames$region_str[region_id]) 
DestinationFile <- paste0(dirs$datadir,"/destinations",region_id,".csv")

dest  <- read.csv(DestinationFile)  
dest  <- dest[dest$shortlist,]
ndest <- nrow(dest)
dest_name  <- strsplit(as.character(dest$name),",")
dest_name  <- matrix(unlist(dest_name),2,nrow(dest))[1,]

# load region shape file
wgs.84          <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
ukgrid <- "+init=epsg:27700"
regionshapefile <- "Regions_December_2015_Generalised_Clipped_Boundaries_in_England"
regions         <- readOGR(dirs$mapdir,layer=regionshapefile)
regions         <- spTransform(regions,CRS(wgs.84))

# 1) Create counterfactual settlement
# 2) Compute new travel times
# 3) Compute new prices
load(paste0(dirs$datadir,"/m2data2.RData"))

  newdata<-C1CreateSettlement(nhouses,popdensity,mapfile,model_laname,m2data)
  newdata<-A4CreateSettlement(region_id,dirs,m2data_new = newdata) 

  out0    <- A6PredictPrice(newdata$m1data_old,newdata$m2data_new,dirs$outdir,m2flag=m2flag)  
  out1    <- A6PredictPrice(newdata$m1data_new,newdata$m2data_new,dirs$outdir,m2flag=m2flag) 
  
  # 1) convex hull of settlement
  houses<-SpatialPoints(newdata$m2data_new[,c("longitude","latitude")],CRS(wgs.84))
  houses<-SpatialPointsDataFrame(houses,data=newdata$m2data_new)
  houses_convexhull<-gConvexHull(houses)
  
  # Create spatial data frames of destionations for plotting
  dest_spatial<-SpatialPoints(dest[,c("longitude","latitude")],CRS(wgs.84))
  dest_spatial<-SpatialPointsDataFrame(dest_spatial,data=dest)
  dest_spatial<-spTransform(dest_spatial,CRS(ukgrid))
  dest_envelope<-gBuffer(gEnvelope(dest_spatial),width=0.1)
  c1<-gUnion(houses_convexhull,spTransform(dest_envelope,CRS(wgs.84)))
  subregion<-gIntersection(regions[region_map_index[region_id],],c1)
  subregion1<-subregion@polygons[[1]]@Polygons[[1]]
 
  # 2) centroid of settlement (for descriptive stats)
  project_centroid<-gCentroid(houses)  
  browser()
  t1<-knn(newdata$m1data_old[,c("longitude","latitude")],project_centroid,newdata$m1data_old$postal_town,k=1) 
  for (i1 in 1:50) {
    project_location_full<-revgeocode(coordinates(project_centroid),source="google",output="more")
    if (!anyNA(project_location_full)) {
      print("Project location address found.")
      break()  
    }
  }
  browser()
  
 if ("postal_town" %in% colnames(project_location_full)) {
    print("Name of nearest postal_town successfully found.")
    project_location<-project_location_full$postal_town
 } else {
   print("Name of nearest town not found. Using default name.")
   project_location<- "Quixotica"
 }
  project_centroid<-coordinates(project_centroid)
  
  # Land area of settlement
  m_sq  <- gArea(spTransform(houses_convexhull,ukgrid))
  km_sq <- m_sq/1e6

  # Load results from models 1 and 2 
  if (m2flag==0) {
    load(paste0(dirs$outdir,"/m2ols0.RData"))
    m2<-m2ols0
  } else if (m2flag==1) {
    load(paste0(dirs$outdir,"/m2ols1.RData"))
    m2<-m2ols1
  }

#    names(logprice)<-c("new","model1_size","model1_location_old","model1_location_new",
#                     "model1_all","model2_new")
  newprice<-exp(out1$new)
  oldprice<-exp(out0$new)
  farmprice_lo  <- rep(m_sq*price_per_sqm[1]/nhouses,length.out=length(newprice))
  farmprice_med <- rep(m_sq*price_per_sqm[2]/nhouses,length.out=length(newprice))
  farmprice_hi  <- rep(m_sq*price_per_sqm[3]/nhouses,length.out=length(newprice))
  
  deltaprice<-newprice-oldprice
  deltaprice_lo<-newprice-farmprice_lo
  deltaprice_med<-newprice-farmprice_med
  deltaprice_hi<-newprice-farmprice_hi
  
