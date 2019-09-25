#' A6PredictPrice(m1data_new,m2data_new,OutDir)
#' Predict transformed prices for model 1 and model 2
#'         (transformed price is one of c("logvalue","value","boxcoxvalue"))
#' 
#' @param m1data_new   data frame for new counterfactual model 1
#' @param m2data_new   data frame for new counteractual model 2
#' @param OutDir       name of output directory            
#' @param m2flag       0 = m2_0, 1 = m2_1, 5 = m2_5
#' @param depvar       dependent variable. one of c("logvalue","value","boxcoxvalue")
#' @return value    data frame containing all price predictions
#' @keywords price prediction
#' @export
#' @examples
#' logprice <-A6PredictPrice(m1data_new,m2data_new,OutDir,m2flag=0,depvar="logvalue")
A6PredictPrice<-function(m1data_new,m2data_new,OutDir,m2flag=0,depvar="logvalue") {
  default_warn<-getOption("warn")
  options(warn=-1)
  # Load m1
  # Load m2ols0
  if (depvar=="logvalue") {
    load(paste0(OutDir,"/m1log0.RData"))
    m1<-m1log0
  } else if (depvar=="value") {
    load(paste0(OutDir,"/m1linear0.RData"))
    m1<-m1linear0
  } else if (depvar=="boxcoxvalue") {
    load(paste0(OutDir,"/m1boxcox0.RData"))
    m1<-m1boxcox0
  }
  if (m2flag==0) {
    load(paste0(OutDir,"/m2",depvar,"0.RData"))
    m2ols<-m2ols0
  } else if (m2flag==1) {
    load(paste0(OutDir,"/m2",depvar,"1.RData"))
    m2ols<-m2ols1
  } else if (m2flag==5) {
    load(paste0(OutDir,"/m2",depvar,"5.RData"))
    m2ols<-m2ols5
  }
  
  # New location_value
  rownames(m2data_new)<-c(1:nrow(m2data_new))
  
  # Convert prob_4band="" to prob_4band==NA
  i1<-(levels(m2data_new$prob_4band)=="")
  levels(m2data_new$prob_4band)[i1]<-NA
  
  newlevels<-levels(m2data_new$prob_4band)
  oldlevels<-levels(m2ols$model$prob_4band)
  if (any(!(newlevels %in% oldlevels))) {
    i1<- (!(newlevels %in% oldlevels))
    levels(m2data_new)[i1]<-NA
  }
  value2<-predict(m2ols,newdata=m2data_new,se.fit=T,interval="prediction",na.action=na.exclude)

  # 1) predict model 1:                  value1
  # 2) predict model 1: location_value:  value1_latitude+value1_longitude
  # 3) predict model 2: location_value:  value2
  # 3) compute value_new                 value1 + (value2-value1_latitude-value1_longitdue)

  # Create splines in new data
  m1data_new$bSize<-bSpline(m1data_new$total_floor_area,
                            knots=m1$knots[[1]],
                            degree=m1$degree[1],intercept=FALSE)
  m1data_new$bLat<-bSpline(m1data_new$latitude,knots=m1$knots[[2]],
                           degree=m1$degree[2],intercept=FALSE)
  m1data_new$bLon<-bSpline(m1data_new$longitude,knots=m1$knots[[3]],
                           degree=m1$degree[3],intercept=FALSE)

  # Indexes in m1pline for (size,latitude,longitude)
  isize   <- grep("bSize",names(m1$coefficients))
  ilat    <- grep("bLat[123456789]",names(m1$coefficients))
  ilon    <- grep("bLon",names(m1$coefficients))
  ilatlon <- grep("bLatLon",names(m1$coefficients))

  value1           <- predict(m1,newdata=m1data_new)
  value1_size      <- m1data_new$bSize %*% 
                      m1$coefficients[isize]
  value1_latitude  <- m1data_new$bLat %*%
                      m1$coefficients[ilat]
  value1_longitude <- m1data_new$bLon %*% 
                      m1$coefficients[ilon]
  location_value   <- value1_latitude+value1_longitude 
  if (m1$LatLonFlag==1) {
    value1_latlon  <- m1data_new$bLatLon %*% m1$coefficients[ilatlon]  
    location_value <- location_value+value1_latlon
  }
  
  # 3) compute value_new:  value_new = lnp1_hat + (g2_hat - g1_hat)
#  value0 <- m2data_new[,grep(paste0("\\<",depvar),colnames(m2data_new))]
  value0 <- m2data_new$location_value

  if (nrow(value2$fit)<length(value1)) {
    value_new <- matrix(NA,length(value1),1)
    i1 <- as.numeric(rownames(value2$fit))
    value2fit <- matrix(NA,length(value1),1)
    value2fit[i1] <- value2$fit
    value_new[i1] <- value1[i1] + (value2$fit[,1] - location_value[i1])
    value     <- data.frame(value_new,value1_size,value0,location_value,
                            value1,value2fit)
  } else {
    value_new <- value1 + (value2$fit[,1] - location_value)
    value     <- data.frame(value_new,value1_size,value0,location_value,
                          value1,value2$fit)
  }
  names(value)<-c("new","model1_size","model1_location_old","model1_location_new",
                  "model1_all","model2_new")
  options(warn=default_warn)
  return(value)
}
