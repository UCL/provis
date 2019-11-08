# Model 2:  Create estimation sample
# Author:   MM,LN
# Version:  2019.11.08
# Revision history
# 2019.11.08   Add loop over values of datastub. produce results for both m11 and nondom.

allDatastubs <- c("m11","nondom")  # "m11"    = domestic
                                   # "nondom" = non-domestic
travelmodelbasis<-"cheb"  # c("cheb","glp","tensor")  basis for travel model

LondonFlag<-0   # 0 : Only Greater London
# 1 : London + some adjacent counties  

N0<-10000      # N0   = size of fullsample used for estimation
               # N0   = 0 then use all data (excluding outliers)

nregs<-11
regnames<-data.frame(region_id=seq(1:nregs), 
                     region_str=c("CaMKOx", "CornwallDevon", 
                                  "EastMid", "EastEng", 
                                  "London", "NE", "NW", 
                                  "SE", "SW", "WestMid", 
                                  "YorkshireHumber"))
wgs.84    <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
ukgrid = "+init=epsg:27700"

for (ds in 1:2) {
  datastub <- allDataStubs[ds]
for (r in 1:11) {
  region_id<-regnames$region_id[r]    
  region_str<-as.character(regnames$region_str[r]) 
  dirs<-B2SetPath(RootDir,CodeDir,DataRoot,region_id,datastub)
  #-----------------------------------------------------------------
  #   Load and clean data
  #-----------------------------------------------------------------
  load(file=paste0(dirs$datadir,"/m2data1.RData"))
  if (N0>0) {
    m2data<-m2data[sample(nrow(m2data),N0),]
  }
 
  if (!exists("m2data$greenbelt")) {
    m2data$greenbelt=0
  } 
#  load(paste0(dirs$datadir,"/region_bbx.R"))
  load(paste0(dirs$datadir,"/convexhull.RData"))  # used to create new variables

  # clean data
  m2data$imddecile<-factor(m2data$imddecile)

  # 1)  travel times to destinations
  #    drive time : dest$shortlist == TRUE
  #    trans time : dest$raildest  == TRUE
  DestinationFile<-paste0(dirs$datadir,"/destinations",region_id,".csv")
  dest<-read.csv(DestinationFile)
  
  # Compute
  # 1) travel times to destinations
  # 2) travel time to postal_town
  # 3) distance to destinations
  # 4) distance to coast
  # 5) distance to AONB
  # 6) distance to national park
  # 7) distance to motorways
  # 8) distance to A road
  # 9) distance to nearest rail station
  # 10) for each house, rank destinations in terms of traveltimes
  m2data<-A2Model2_newvariables(m2data,dest,convexhull,region_id,dirs,drop_outlier=TRUE,
                                basis = travelmodelbasis)

  # 10) Create splines and save spline details
  idrive<-grep("\\<drive_",colnames(m2data))
  itrans<-grep("\\<trans_",colnames(m2data))
  iavgtime <- grep("\\<avgtime_",colnames(m2data))
  m2dataspline<-list(c(colnames(m2data)[idrive],colnames(m2data)[itrans],
                       colnames(m2data)[iavgtime]))
  names(m2dataspline)[1]<-"varlist"
  m2dataspline$degree<-rep(1,length(m2dataspline$varlist))
  m2dataspline$segments <- rep(4,length(m2dataspline$varlist))
  m2dataspline$knots    <- vector("list",length(m2dataspline$varlist))
  
  for (i1 in 1:length(m2dataspline$varlist)) {
    i2<-grep(m2dataspline$varlist[i1],colnames(m2data))
    tempknots<-quantile(m2data[,m2dataspline$varlist[i1]],
                        seq(0.1,0.9,length.out=m2dataspline$segments[i1]),
                        na.rm=TRUE)
    m2dataspline$knots[[i1]]<-tempknots
    names(m2dataspline$knots)[i1]<-m2dataspline$varlist[i1]
  
    m2data$tempB<-bSpline(m2data[,m2dataspline$varlist[i1]],
                          knots=tempknots,
                        degree=m2dataspline$degree[i1],intercept=FALSE)
    iTempB<-grep("tempB",colnames(m2data))
    names(m2data)[iTempB]<-paste0("spline_",m2dataspline$varlist[i1])
  }
  save(m2dataspline,file=paste0(dirs$outdir,"/m2dataspline.RData"))
    
#-----------------------------------------------------------------
#  Save data
#-----------------------------------------------------------------

save(m2data,file=paste0(dirs$datadir,"/m2data2.RData"))

}  # loop over regions
}  # loop over c("domestic","nondomestic") data
