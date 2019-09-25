#' Create base dataframe for new settlement, based on user inputs
#' @param nhouses       Number of new houses to build
#' @param popdensity    Population density in new settlement
#' @param mapfile       kml file with coordinates defining boundary of new settlement
#' @param model_laname  laname used as sample to draw house and location variables
#' @param m2data        dataframe containing all variables needed to estimate models 1 and 2
#' @return newdata      dataframe for new settlement
#' @examples
#' nhouses      <- 1000
#' popdensity   <- 35
#' mapfile      <- "gotham.kml"
#' model_laname <- "Nottingham"
#' load("m2data.RData")
#' newdata <- C1CreateSettlement(nhouses,popdensity,mapfile,model_laname,m2data)

C1CreateSettlement<-function(nhouses,popdensity,mapfile,model_laname,m2data) {

# Load mapfile
  layer      <- ogrListLayers(mapfile)
  settlement <- readOGR(mapfile,layer=layer)
  
  # extract subset of data from m2data for use as model for new settlement
  imodel    <- (levels(m2data$lad15nm)[m2data$lad15nm]==model_laname)
  modeldata <- m2data[imodel,]
  
  # 1)  Create (longitude,latitude) using spsample(...)
  #     type = "regular"  regularly spaced points
  set.seed(345)
  newdata <- data.frame(coordinates(spsample(settlement,round(1.2*nhouses,0),type="regular")))
  if (nrow(newdata)>nhouses) {
    newdata<-newdata[1:nhouses,]
  } else if (nrow(newdata)<nhouses) {
    nhouses = nrow(newdata)  
  }
  names(newdata) <- c("longitude","latitude")  
  
  set.seed(436532)
  isample<-sample(c(1:nrow(modeldata)),size = nhouses,replace=TRUE)

  # 2) (propertytype,tenure,total_floor_area)
  varlist <- c("propertytype","tenure","total_floor_area")
  newdata <- data.frame(newdata,modeldata[isample,varlist])
    
  # add land use variables
  # TODO
  #   replace NA values in landuse variables
  i1      <- grep("\\<lu_",colnames(modeldata))
  newdata <- data.frame(newdata,modeldata[isample,i1])
  
  # popdensityOA
  newdata$popdensityOA <- popdensity
  
  #laname
  laname <-knn(m2data[,c("longitude","latitude")],newdata[,c("longitude","latitude")],
               m2data$laname,k=1)
  newdata$laname <- laname
  
  # Set (localplanrate,builtupland_pct,busyland_pct) = average value in relevant local authority
  newdata$localplanrate   <- NA
  newdata$busyland_pct    <- NA
  newdata$builtuparea_pct <- NA
  for (i1 in levels(laname)) {
    inew<-(levels(newdata$laname)[newdata$laname]==i1)
    if (sum(inew)>0) {
      iold            <- (levels(m2data$laname)[m2data$laname]==i1)
      localplanrate   <- mean(m2data$localplanrate[iold])
      builtuparea_pct <- mean(m2data$builtuparea_pct[iold])
      busyland_pct    <- mean(m2data$busyland_pct[iold])
    
      newdata$localplanrate[inew]   <- localplanrate
      newdata$builtuparea_pct[inew] <- builtuparea_pct
      newdata$busyland_pct[inew]    <- busyland_pct
    }
  }
  return(newdata)
}
