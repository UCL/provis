#' heatmap1(x,y,z,filter,resolution,zlabel,outfile)
#' This function plots a heatmap for z = f(x,y)
#'
#' @param x        vector of \code{x} coordinates
#' @param y        vector of \code{y} coordinates
#' @param z        vector of \code{z} coordinates
#' @param filter   logical to subset \code{(x,y,z)}
#' @param zlabel   string   label for \code{z}
#' @param outfile  name of output file
#' @param places   n x 3 matrix of places, (name,lon,lat)
#' @param route    n x 3 matrix of route, (name,lon,lat)
#' @param routelabel "road" or "rail"
#' @return plot1   details of plot
#' @keywords heatmap
#' @export
#' @examples
#' h<-heatmap1(x,y,z,filter=isample,resolution=0.005,zlabel="z",outfile="h1.eps")
heatmap1<- function(x,y,z,filter,resolution,zlabel,outfile,
                    places,route,routelabel) {
  if (missing(filter)) {
    filter <- !is.na(z)
  }

  x<-x[filter]
  y<-y[filter]
  z<-z[filter]
    map1 <- interp(x=x, y=y,z=z,
                 yo=seq(min(y),max(y),by=resolution),
                 xo=seq(min(x),max(x),by=resolution),
                 duplicate="mean")

  if (missing(places) & missing(route)) {
    filled.contour(map1, color.palette=terrain.colors,
                          plot.title={
                          title(xlab="Longitude",cex.lab=1)
                           mtext("Latitude",2,cex=1,line=3,las=0)
                        mtext(zlabel,4,cex=1,line=0.8,las=0)
                       })
  } else if (!missing(places) & missing(route)) {
    filled.contour(map1, color.palette=terrain.colors,
                   plot.title={
                     title(xlab="Longitude",cex.lab=1)
                     mtext("Latitude",2,cex=1,line=3,las=0)
                     mtext(zlabel,4,cex=1,line=0.8,las=0)},  
                   plot.axes={points(places[,2:3], pch=24); 
                              text(places[,2:3], pos=1, labels=places[,1], cex=0.7)})
  } else if (missing(places) & !missing(route)) {
    filled.contour(map1, color.palette=terrain.colors,
                   plot.title={title(xlab="Longitude",cex.lab=1)
                     mtext("Latitude",2,cex=1,line=3,las=0)
                     mtext(zlabel,4,cex=1,line=0.8,las=0)},
                   plot.axes={lines(route[,2:3], col="black"); 
                              points(route[,2:3],pch=1);
                              text(route[1,2:3],pos=1,labels=routelabel,cex=0.7)})
  } else if (!missing(places) & !missing(route)) {
    filled.contour(map1, color.palette=terrain.colors,
                   plot.title={title(xlab="Longitude",cex.lab=1)
                               mtext("Latitude",2,cex=1,line=3,las=0)
                               mtext(zlabel,4,cex=1,line=0.8,las=0)},
                   plot.axes={points(places[,2:3], pch=24); 
                              text(places[,2:3], pos=1, labels=places[,1], cex=0.7);
                              lines(route[,2:3],col="black");
                              text(route[1,2:3],pos=1,labels=routelabel,cex=0.7)})
  }

  dev.copy(pdf,outfile)
  dev.off()
}


