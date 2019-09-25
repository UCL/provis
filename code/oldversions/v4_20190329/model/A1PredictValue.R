A1PredictValue <- function(m1,m1data,LatLonFlag) {
  
  # Find indexes for spline coefficients
  isize<-grep("bSize",names(m1$coefficients))
  ilat<-grep("bLat[123456789]",names(m1$coefficients))
  ilon<-grep("bLon",names(m1$coefficients))
  ilatlon<-grep("bLatLon",names(m1$coefficients))

  # Predict:
  #  1) y0 = predicted log price
  #  2) y1 = component of log price that varies with total_floor_area
  #  3) y2 = component of log price that varies with latitude
  #  4) y3 = component of log price that varies with longitude
  y0<-predict(m1,newdata=m1data)
  y1<-m1data$bSize %*%  m1$coefficients[isize]
  y2<-m1data$bLat  %*%  m1$coefficients[ilat]
  y3<-m1data$bLon  %*%  m1$coefficients[ilon]
  if (LatLonFlag==1) y4<-m1data$bLatLon %*% m1$coefficients[ilatlon]  

  # Recovering g(e,n)
  structure_value<- y1
  location_value<-y2+y3
  if (LatLonFlag==1) {
    location_value <- location_value+y4
  }
  A1PredictValue<-data.frame(location_value,structure_value)
  return(A1PredictValue)
}