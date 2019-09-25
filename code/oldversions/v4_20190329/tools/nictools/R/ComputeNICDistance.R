#' ComputeNICDistance(newdata,dest)
#' Compute distances from each observation in newdata to each destination in dest.
#'
#' @param newdata  dataframe containing (latitude,longitude)
#' @param dest     dataframe containing [dest_name,dest_name_short,latitude,longitude]
#' @return newdata dataframe containing results
#' @keywords geographic distance
#' @export
#' @examples
#' newdata<-ComputeNICDistance(olddata,dest)
ComputeNICDistance<- function(newdata,dest) {

ndest<-nrow(dest)
n<-ncol(newdata)
ilat<-grep("\\<latitude\\>",colnames(newdata))
ilon<-grep("\\<longitude\\>",colnames(newdata))
ilat_dest<-grep("latitude",colnames(dest))
ilon_dest<-grep("longitude",colnames(dest))
iname<-grep("shortname",colnames(dest))

for (i1 in 1:ndest) {
  n<-n+1
  assign("x",distGeo(newdata[,c(ilon,ilat)],dest[i1,c(ilon_dest,ilat_dest)])/1000)
  newdata$tempx<-x
  names(newdata)[n]<-paste0("distance_",dest[i1,iname])
}
return(newdata)
}
