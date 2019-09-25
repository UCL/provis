# plot price vs total_floor_area
for (r in 1:nregions) {
  # Find indexes for spline coefficients
  isize<-grep("bSize",names(m1[[r]]$coefficients))
  ilat<-grep("bLat[123456789]",names(m1[[r]]$coefficients))
  ilon<-grep("bLon",names(m1[[r]]$coefficients))
  ilatlon<-grep("bLatLon",names(m1[[r]]$coefficients))
  # Predict:
  #  1) logprice0 = predicted log price
  #  2) logprice1 = component of log price that varies with total_floor_area
  #  3) logprice2 = component of log price that varies with latitude
  #  4) logprice3 = component of log price that varies with longitude
  logprice0<-predict(m1[[r]])
  logprice1<-m1[[r]]$model$bSize %*% 
    m1[[r]]$coefficients[isize]
  logprice2<-m1[[r]]$model$bLat %*%
    m1[[r]]$coefficients[ilat]
  logprice3<-m1[[r]]$model$bLon %*% 
    m1[[r]]$coefficients[ilon]
  logprice4<-m1[[r]]$model$bLatLon %*% m1[[r]]$coefficients[ilatlon]  
  
  # Recovering g(e,n)
  location_value<-logprice2+logprice3+logprice4
  
  std_lv<-(location_value-mean(location_value))/sd(location_value)
  
  resolution <- 0.5 
  map1 <- interp(x=longitude, y=latitude, z=std_lv, 
                 yo=seq(min(latitude),max(latitude),by=resolution), 
                 xo=seq(min(longitude),max(longitude),by=resolution), duplicate="mean")
  if (plotflag==2) {
    postscript(file=paste0(dirs$outdir,'/map5f_model1.eps'))
  }
  filled.contour(map5f, color.palette=rainbow, 
                 plot.title={
                   title(xlab="Longitude",cex.lab=1)
                   mtext("Latitude",2,cex=1,line=3,las=0)
                   mtext("Standardized location value",4,cex=1,line=0.8,las=0)
                 }, plot.axes={points(dest[dest$shortlist,3:4], pch=24); axis(1); axis(2); 
                   text(dest[dest$shortlist,3:4], pos=1, labels=dest[dest$shortlist,2], cex=0.7);
                 })
  if (plotflag==1) {
    dev.copy2eps(file=paste0(dirs$outdir,'/map5f_model1.eps'))
    dev.copy2pdf(file=paste0(dirs$outdir,'/map5f_model1.pdf'))
  }
  dev.off()
}