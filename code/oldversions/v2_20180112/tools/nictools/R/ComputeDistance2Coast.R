#' Compute distance to coast
#' @param location   vector of locations (longitude,latitude)
#' @param mapdir     directory containing shape file with england coastline
#' @return distance  distance to coast and coordinates of nearest point
#' @export
#' @examples
#' dist <- ComputeDistance2Coast(location,mapdir)
ComputeDistance2Coast<-function(location,mapdir) {

  # Load shape file with coast
  coastfile<-"englandcoast"
  coast<-readOGR(mapdir,layer=coastfile)
  distance<-dist2Line(location,coast)
  return(distance)
}



