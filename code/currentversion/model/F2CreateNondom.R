#' F2CreateNondom(region,dirs,filename,title,outfile,m2data_new)
#' Create data frames for new nondomestic properties and predict prices
#' 
#' @param region       id number for region
#' @param dirs            list of directories
#' @param filename        .csv file containing details of settlement
#' @param m2data_new      input base dataframe for new settlement
#' @param depvar          dependent variable. one of c("logvalue","value","boxcoxvalue")
#' @param travelmodelbasis c("cheb","glp","tensor")
#' @param title           title of table of summary stats
#' @param outfile         output file
#' @return newdata        list containing (m1data_new,m2data_new)
#' @keywords new settlement
#' @export
#' @examples
#' newdata <- F2CreateNondom(region = 1,dirs,filename = "Bedford.csv",title,outfile)
#' newdata <- F2CreateNondom(region = 1,dirs,m2data_new = m2data_base)
F2CreateNondom<-function(region,dirs,filename,m2data_new,depvar="logvalue",travelmodelbasis="cheb",
                         title="none",outfile="none") {
  default_warn<-getOption("warn")
  options(warn=-1)
  # proj_4string for spatial objects
  wgs.84    <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

  if (!missing(filename)) {
    # Load New settlement data
  #  example:  filename<-"Bedford.csv"  
    m2data_new<-read.csv(paste0(dirs$newdatadir,"/",filename), header=TRUE)
  } else if (missing(filename) & missing(m2data_new)) {
    stop("Error in F2CreateNondom. Must input either 'filename' or 'm2data_new'.")
  }
  DestinationFile <- paste0(dirs$datadir,"/destinations",region,".csv")
  dest            <- read.csv(DestinationFile)
  
  load(paste0(dirs$newdatadir,"/m1data_template.RData"))
  load(paste0(dirs$datadir,"/m2data2.RData"))
  load(paste0(dirs$datadir,"/convexhull.RData"))  # used for A2Model2_newvariables
  
  # set location value to one of c("logvalue","value","boxcoxvalue")
  m2data$location_value <- m2data[,grep(paste0("\\<",depvar),colnames(m2data))]
  
  #levels(m2data_new$tenure)<-c("F","L")

  # create dataset to estimate model 1
  if (nrow(m1data_template)>nrow(m2data_new)) {
    m1data_new<-m1data_template[1:nrow(m2data_new),]
  } else {
    m1data_new<-m1data_template[sample(c(1:nrow(m1data_template)),nrow(m2data_new),replace=TRUE),]  
  }  
  m1data_new$usecode          <- m2data_new$usecode
  m1data_new$total_floor_area <- m2data_new$total_floor_area
  m1data_new$latitude         <- m2data_new$latitude
  m1data_new$longitude        <- m2data_new$longitude
  
  # Convert prob_4band="" to prob_4band==NA
  i1<-(levels(m1data_new$prob_4band)=="")
  levels(m1data_new$prob_4band)[i1]<-NA
  
  # create interaction (lat_long)
  m1data_new               <- cbind(m1data_new,m1data_new$latitude*m1data_new$longitude)
  i1                       <- grep("m1data*",colnames(m1data_new))
  colnames(m1data_new)[i1] <- "lat_long"
  
  #  predict location_value for new settlement
  #    iold = indexes of points in m2data that are in the region defined by envelope of m2data_new
  p1<-SpatialPoints(m2data_new[,c("longitude","latitude")],CRS(wgs.84))
  p2<-SpatialPoints(m2data[,c("longitude","latitude")],CRS(wgs.84))
  
  poly1<-gEnvelope(p1)
  poly2<-gBuffer(poly1,width=0.1)
  
  iold<-gIntersects(poly1,p2,byid=TRUE)
  if (sum(iold)<100) {
      iold<-gIntersects(poly2,p2,byid=TRUE)
  }
  if (sum(iold)<100) {
      laname<-droplevels(m2data_new$laname)
      iold <- rep(FALSE,length=nrow(m2data))
      for (i1 in levels(laname)) {
          itemp <- levels(m2data$laname)[m2data$laname]==i1
          iold[itemp]<-TRUE
      }
  }
  tempdata<-m2data[iold[,1],]
  if (nrow(tempdata)>1000) {
      isample<-sample(c(1:nrow(tempdata)),size=1000,replace=F)
  }else {
      isample<-c(1:nrow(tempdata))
  }
  
  np_location_value         <-npreg(location_value~latitude+longitude,data=tempdata[isample,])
  m2data_new$location_value <-predict(np_location_value,newdata=m2data_new)

  # Restricted land percentage
  m2data_new$restrictedland_pct<-mean(tempdata$restrictedland_pct)
  
  # 1) imdrank
  # 2) imddecile
  # 3) prob_4band
  # 4) noiseclass
  # 5) roadnoise
  # 6) greenbelt
  i1<-sample(c(1:sum(iold)),size=nrow(m2data_new),replace=TRUE)
  m2data_new$imdrank<-tempdata$imdrank[i1]
  m2data_new$imddecile<-tempdata$imddecile[i1]
  m2data_new$prob_4band<-tempdata$prob_4band[i1]
  # Convert prob_4band="" to prob_4band==NA
  iprob_4band<-(levels(m2data_new$prob_4band)=="")
  levels(m2data_new$prob_4band)[iprob_4band]<-NA
  
  m2data_new$roadnoise<-tempdata$roadnoise[i1]
  m2data_new$greenbelt<-tempdata$greenbelt[i1]

  # Impute postal_town and (town_lon,town_lat) using tempdata
  postal_town<-knn(tempdata[,c("longitude","latitude")],m2data_new[,c("longitude","latitude")],
          tempdata[,"postal_town"],k=1)
  postal_town<-data.frame(postal_town)
  names(postal_town)<-"postal_town"
  postal_town<-join(postal_town,tempdata[,c("postal_town","town_lon","town_lat")],
                    by="postal_town",type="left",match="first")
  m2data_new[,c("postal_town","town_lon","town_lat")]<-postal_town
  
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
  m2data_new<-A2Model2_newvariables(m2data_new,dest,convexhull,region,dirs,
                                    drop_outlier=FALSE,basis=travelmodelbasis)

  # load spline details and compute splines
  load(file=paste0(dirs$outdir,"/m2dataspline.RData"))
  for (i1 in 1:length(m2dataspline$varlist)) {
    i2<-grep(m2dataspline$varlist[i1],colnames(m2data_new))
    tempknots<-m2dataspline$knots[[i1]]
    m2data_new$tempB<-bSpline(m2data_new[,m2dataspline$varlist[i1]],
                              knots=tempknots,
                              degree=m2dataspline$degree[i1],intercept=FALSE)
    iTempB<-grep("tempB",colnames(m2data_new))
    names(m2data_new)[iTempB]<-paste0("spline_",m2dataspline$varlist[i1])
  }
  
  # summary statistics
  if (title!="none") {
    stargazer(tempdata[,m2ols0$varlist],
              m2data_new[,m2ols0$varlist],title=title,
              align=TRUE, out.header=TRUE,digits=4,
              out=paste0(dirs$outdir,"/",outfile))
  }
  load(file=paste0(dirs$datadir,"/m1data.RData"))
  if (nrow(m1data)>nrow(m1data_new)) {
    # Extract subset of m1data same size as m1data_new
    n1<-nrow(m1data)
    n2<-nrow(m1data_new)
    set.seed(569472)
    i1<-sample(c(1:n1),size=n2)
    m1data<-m1data[i1,]
  } else if (nrow(m1data_new)>nrow(m1data)) {
    # m1data_new has more rows than m1data
    # create sample (with replacement) from m1data and 
    # set size equal to size of m1data_new
    n1<=nrow(m1data_new)
    n2<-nrow(m1data)
    set.seed(569471)
    i1<-sample(c(1:n2),size=n1,replace=TRUE)
    m1data<-m1data[i1,]
  }
  
  outdata<-list(m1data_new,m2data_new,m1data)
  names(outdata)<-c("m1data_new","m2data_new","m1data_old")
  options(warn=default_warn)
  return(outdata)
}
