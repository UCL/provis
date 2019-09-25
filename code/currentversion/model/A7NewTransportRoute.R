#' A7NewTransportRoute(region_id,routefile,speed,dirs)
#' Predict travel times with new tranport route (road/rail) and create dataframe with results
#'
#' @param region_id       region id number
#' @param routefile       filename containing route coordinates (road junctions or rail stations)
#' @param travelmode     "drive" or "trans"
#' @param speed           average speed on new road/rail route, miles per hour
#' @param dirs            list of directories
#' @param usemapdir       TRUE then use mapdir in pathname
#' @param travelmodelbasis c("cheb","glp","tensor")
#' @param m2data          baseline m2data dataframe
#' @return outdata        list containing (m1data_new,m2data_new)
#' @keywords new settlement
#' @export
#' @examples
#' newdata <- A7NewTransportRoute(region_id,routefile,travelmode="drive",speed,dirs)
#' newdata <- A7NewTransportRoute(region_id,routefile,travelmode="trans",speed,dirs)

A7NewTransportRoute<-function(region_id,routefile="region1_road1.kml",travelmode="drive",
                    speed=80,dirs,usemapdir=TRUE,travelmodelbasis="cheb",m2data=NULL) {

  speed<-1.609*speed  # convert to kilometers per hour

  # Load m2data
  if (is.null(m2data)) {
    load(paste0(dirs$datadir,"/m2data2.RData"))
  }
  
  #4) load kml file in r
  if (usemapdir) {
    routefile<-paste0(dirs$mapdir,"/",routefile)
  } else {
    routefile<-routefile
  }  
  layer<-ogrListLayers(routefile)
  route<-readOGR(routefile,layer=layer)
  # convert route to ("name","shortname","longitude","latitude","shortlist")
  if (class(route)=="SpatialPointsDataFrame") {
    route<-as.data.frame(route)
  } else if (class(route)=="SpatialLinesDataFrame") {
    route<-fortify(route)  
  }  
  route$name<-paste0("J",as.character(c(1:nrow(route))))
  route$shortname<-route$name
  
  ilon <- grep("\\<coords.x1",colnames(route))
  if (length(ilon)==0) {
    ilon <- grep("\\<lon",colnames(route))
  }
  ilat <- grep("\\<coords.x2",colnames(route))
  if (length(ilat)==0) {
    ilat <- grep("\\<lat",colnames(route))
  }
  colnames(route)[ilon]<-"longitude"
  colnames(route)[ilat]<-"latitude"
  
  route$shortlist<-TRUE
  
  # Load destinations
  DestinationFile<-paste0(dirs$datadir,"/destinations",region_id,".csv")
  dest<-read.csv(DestinationFile)
  dest<-dest[dest$shortlist,]
  ndest<-sum(dest$shortlist)

  # Predict travel times:
  # ttime1:  travel time from each property to each point on route
  #         (i.e.  to each road junction or to each rail station)
  # ttime2: travel time from each route junction to each destination
  # TravelTime from N houses to K route junctions (road junctions or rail stations)
  ttime1  <-predictTravelTime(origin = m2data[,c("longitude","latitude")],
                              dest   = route,
                              region_id,dirs$traveldir,travelmode=travelmode,
                              drop_outlier = FALSE,basis=travelmodelbasis)
  namelist<-colnames(ttime1)
  for (j1 in 1:nrow(route)) {
    if (any(is.na(ttime1[,j1+2]))) {
      tempdata<-ttime1[!is.na(ttime1[,j1+2]),namelist[c(1,2,j1+2)]]
      newdata<-ttime1[is.na(ttime1[,j1+2]),namelist[c(1,2,j1+2)]]
      if (nrow(tempdata)>1000) {
        set.seed(534)
        tempdata<-tempdata[sample(nrow(tempdata), 1000), ]
      }
      np1<-npreg(as.formula(paste0(namelist[j1+2],"~longitude+latitude")),data=tempdata,
                 newdata=newdata)
      ttime1[is.na(ttime1[,j1+2]),j1+2]<-np1$mean
    }
  }
  # Use google to fill in missing values
  #ttime1<-GetMissingTravelTime(ttime1,route,mode="driving",FindDestName=TRUE)
    
  # travel time from K route junctions to L destinations
  ttime2 <- predictTravelTime(origin = route[,c("longitude","latitude")],
                              dest   = dest,
                              region_id,dirs$traveldir,travelmode=travelmode,
                              drop_outlier = FALSE,
                              basis=travelmodelbasis)
  # Use google to fill in missing values
  ttime2<-GetMissingTravelTime(ttime2,dest,mode=travelmode,FindDestName=FALSE)
  
  # Compute minimum travel to/from route
  # select relevant columns of ttime1 to compute minimum
  imode<-grep(paste0(travelmode,"_"),colnames(ttime1))
  ttime1<-ttime1[,imode]
  mintime_toroute<-apply(ttime1,1,min)
  
  # select relevant columns of ttime2 to compute minimum
  imode<-grep(paste0(travelmode,"_"),colnames(ttime2))
  ttime2<-ttime2[,imode]
  mintime_todest<-apply(ttime2,2,min)

  # Find junction/station that corresponds to minimum travel time
  junction_toroute<-apply(ttime1,1,which.min)
  junction_todest<-apply(ttime2,2,which.min)

  # Compute distance travelled along route for each destination
  start  <- as.data.frame(route[junction_toroute,c("longitude","latitude")])
  routedistance<-matrix(NA,nrow(start),ndest)
  for (i1 in 1:ndest) {
    finish<-route[junction_todest[i1],c("longitude","latitude")]
    finish$shortname<-dest$shortname[i1]
    temp<-ComputeNICDistance(start,finish)
    routedistance[,i1]<-temp[,3]
  }

  #6) compute new travel times
  #       a) current travel times
  #       b) new travel time
  #           i) min travel time to junction/station on route and id of junction i)
  #          ii) min travel time from route to destinations and id of junction ii)
  #          iii) travel time to route + along route + from route to destination
  #       c) new time = min(a,b)

  ispline  <- grep(paste0("spline_",travelmode,"_"),colnames(m2data))
  ispline2 <- grep("spline_avgtime_",colnames(m2data))
  m2data<-m2data[,-c(ispline,ispline2)]
  
  # all columns of m2data that relate to "drive_" excluding "drive_xxx"
  # where xxx is any word with lowercase letter
  # also exclude AONB
  imode<-grep(paste0("\\<",travelmode,"_[^acmnst]"),colnames(m2data))
  iAONB<-grep(paste0("\\<",travelmode,"_AONB"),colnames(m2data))
  if (length(iAONB)>0) {
    itemp<- (imode != iAONB)
    imode<-imode[itemp]
  }
  traveltime1<-m2data[,imode]
  traveltime2<-matrix(NA,nrow(m2data),sum(dest$shortlist))
  for (i1 in 1:length(imode)) {
    traveltime2[,i1]<- mintime_toroute +
                      60*routedistance[,i1]/speed +
                      mintime_todest[i1]
    m2data[,imode[i1]]<-pmin(traveltime1[,i1],traveltime2[,i1])
  }
  
  # update average drive time and probdrive  
  iavgtime<-grep("\\<avgtime_",colnames(m2data))
  iprobdrive<-grep("probdrive_",colnames(m2data))
  m2data<-m2data[,-c(iavgtime,iprobdrive)]
  m2data<-ComputeAverageTime(m2data)
  
  return(m2data)
}

