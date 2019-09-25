#' GetMissingTravelTime(ttime1,dest,mode)
#' replace missing travel times with values from google
#'
#' @param ttime1          (n x K+2) matrix of coorindates and travel times [long,lat,t1,...,tK]
#' @param dest            (K x ..) matrix of coordinates of destinations
#' @param mode            "drive" or "trans"
#' @param FindDestName    TRUE = use Google to find dest name. FALSE = use existing dest name
#' @return ttime1         new version of ttime1
#' @keywords traveltime Google
#' @export
#' @examples
#' ttime1 <- GetMissingTravelTime(ttime1,roads,"drive")
#' ttime1 <- GetMissingTravelTime(ttime1,dest,"trans")

GetMissingTravelTime<-function(ttime1,dest,mode="drive",FindDestName=TRUE) {

  if (mode=="drive") {
    mode<-"driving"
  } else {
    mode<-"transit"
  }
  # if is.na(ttime1), get traveltime from google
  for (j1 in 1:nrow(dest)) {
    if (any(is.na(ttime1[,j1+2]))) {
      i1<-c(1:nrow(ttime1))[is.na(ttime1[,j1+2])]
      # Default departure time  
      dtime<-as.numeric(strptime("2018-03-05 09:00:00",format="%Y-%m-%d %H:%M:%S"))
      if (FindDestName) {
        dest$name<-NA
        for (j2 in 1:50) {
          dname<-try(revgeocode(as.matrix(dest[j1,c("longitude","latitude")]),output="more",
                            messaging=FALSE))
          if ("address" %in% names(dname)) {
            dest$name[j1]<-as.character(dname$address)
            break()
          }
        }
      }
      
      for (i2 in i1) {
        for (j2 in 1:50) {
          orig<-try(revgeocode(as.matrix(ttime1[i2,1:2]),output="more",
                           messaging=FALSE))
          if ("address" %in% names(orig)) {
            break()
          }
        }
        for (j2 in 1:50) {
          temptime<-try(route(from=as.character(orig$address),
                              to=as.character(dest$name[j1]),mode=mode,output="simple",
                              departure_time=dtime,structure="route",
                              override_limit=TRUE,
                              messaging=FALSE))
          if ("minutes" %in% names(temptime)) {
            ttime1[i2,j1+2]<-sum(temptime$minutes,na.rm=TRUE)  
            break()
          }
        }
      }
    }
  }
  return(ttime1)
}