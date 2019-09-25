#' A5PredictPrice(m1data_new,m2data_new,OutDir)
#' Predict prices for model 1 and model 2
#' 
#' @param m1data_new   data frame for new counterfactual model 1
#' @param m2data_new   data frame for new counteractual model 2
#' @param OutDir       name of output directory            
#' @param m2flag       0 = m2ols0, 1 = m2ols1
#' @return logprice    data frame containing all price predictions
#' @keywords price prediction
#' @export
#' @examples
#' logprice <-A5PredictPrice(m1data_new,m2data_new,OutDir)
A6PredictPrice<-function(m1data_new,m2data_new,OutDir,m2flag=0) {

  default_warn<-getOption("warn")
  options(warn=-1)

  # Load m1spline0
  # Load m2ols0
  load(paste0(OutDir,"/m1spline0.RData"))
  if (m2flag==0) {
    load(paste0(OutDir,"/m2ols0.RData"))
    m2ols<-m2ols0
  } else if (m2flag==1) {
    load(paste0(OutDir,"/m2ols1.RData"))
    m2ols<-m2ols1
  }
  
  # New location_value
  logprice2<-predict(m2ols,newdata=m2data_new,se.fit=T,interval="prediction")

  # 1) predict model 1:                  logprice1
  # 2) predict model 1: location_value:  logprice1_latitude+logprice1_longitude
  # 3) predict model 2: location_value:  logprice2
  # 3) compute logprice_new              logprice1 + (logrice2-logprice1_latitude-logprice1_longitdue)

  # Create splines in new data
  m1data_new$bSize<-bSpline(m1data_new$total_floor_area,
                            knots=m1spline0$knots[[1]],
                            degree=m1spline0$degree[1],intercept=FALSE)
  m1data_new$bLat<-bSpline(m1data_new$latitude,knots=m1spline0$knots[[2]],
                           degree=m1spline0$degree[2],intercept=FALSE)
  m1data_new$bLon<-bSpline(m1data_new$longitude,knots=m1spline0$knots[[3]],
                           degree=m1spline0$degree[3],intercept=FALSE)

  # Indexes in m1pline for (size,latitude,longitude)
  isize<-grep("bSize",names(m1spline0$coefficients))
  ilat<-grep("bLat[123456789]",names(m1spline0$coefficients))
  ilon<-grep("bLon",names(m1spline0$coefficients))
  ilatlon<-grep("bLatLon",names(m1spline0$coefficients))

  logprice1<-predict(m1spline0,newdata=m1data_new)
  logprice1_size<-m1data_new$bSize %*% 
                  m1spline0$coefficients[isize]
  logprice1_latitude<-m1data_new$bLat %*%
                      m1spline0$coefficients[ilat]
  logprice1_longitude<-m1data_new$bLon %*% 
                       m1spline0$coefficients[ilon]
  location_value<-logprice1_latitude+logprice1_longitude 
  if (m1spline0$LatLonFlag==1) {
    logprice1_latlon <- m1data_new$bLatLon %*% m1spline0$coefficients[ilatlon]  
    location_value   <- location_value+logprice1_latlon
  }
  
  # 3) compute logprice_new:  logprice_new = lnp1_hat + (g2_hat - g1_hat)
  logprice_new<-logprice1 + (logprice2$fit[,1] - location_value)
 
  logprice0<-m2data_new$location_value
  logprice<-data.frame(logprice_new,logprice1_size,logprice0,location_value,
                       logprice1,logprice2$fit)
  names(logprice)<-c("new","model1_size","model1_location_old","model1_location_new",
                     "model1_all","model2_new")
  options(warn=default_warn)
  return(logprice)
}
