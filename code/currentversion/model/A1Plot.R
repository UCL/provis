A1Plot<-function(location_value,structure_value,m1data,dest,vname,
                 lambda,dirs,plotflag,resolution=0.05) {
  tempfile1<-paste0(dirs$outdir,'/m1_density_',vname,'.eps')
  tempfile2<-paste0(dirs$outdir,'/m1_heatmap_',vname,'.eps')
  tempfile3<-paste0(dirs$outdir,"/m1_size_",vname,".eps")
  tempfile4<-paste0(dirs$outdir,"/m1_size_price.eps")
  
  #browser()
  # Plot 1: Plot density of location value
  if (plotflag==2) postscript(tempfile1)
  plot(density(location_value,na.rm=TRUE), main=expression(g(e[i],n[i])))
  if (plotflag==1) dev.copy(postscript,tempfile1)
  dev.off()
  
  
  # Plot 2: plot heatmap of location value
  std_lv<-(location_value-mean(location_value))/sd(location_value)
#  resolution <- 0.05
  map1 <- interp(x=m1data$longitude, y=m1data$latitude, z=std_lv, 
                 yo=seq(min(m1data$latitude),max(m1data$latitude),by=resolution), 
                 xo=seq(min(m1data$longitude),max(m1data$longitude),by=resolution), duplicate="mean")
  if (plotflag==2) postscript(file=tempfile2)
  filled.contour(map1, color.palette=rainbow, 
                 plot.title={
                   title(xlab="Longitude",cex.lab=1)
                   mtext("Latitude",2,cex=1,line=3,las=0)
                   mtext(paste0("Standardized ",vname),4,cex=1,line=0.8,las=0)
                 }, plot.axes={points(dest[dest$shortlist,3:4], pch=24); axis(1); axis(2); 
                   text(dest[dest$shortlist,3:4], pos=1, labels=dest[dest$shortlist,2], cex=0.7);
                 },nlevels=30)
  if (plotflag==1) dev.copy(postscript,tempfile2)
  dev.off()
  
  # Plot 3: plot location_value vs size
  #         also plot price vs size if location_value != price
  if (plotflag==2) postscript(tempfile3)
  plot(m1data$total_floor_area,structure_value-min(structure_value),pch=".",
       xlab="total floor area (sq. m.)",
       ylab="structure_value")
  if (plotflag==1) dev.copy(postscript,tempfile3)
  dev.off()
  
  i1<-m1data$total_floor_area>45 & m1data$total_floor_area<=55
  p0    <- mean(m1data$price[i1])
  if (vname=="logvalue") {
    price <- exp(structure_value-min(structure_value))*p0/1000  
  } else if (vname=="value") {
    price <- structure_value - min(structure_value) + p0/1000
  } else if (vname=="boxcoxvalue") {
    x0    <- ((p0/1000)^lambda-1)/lambda
    price <- (1+lambda*(structure_value-min(structure_value)+x0))^(1/lambda)  
  }
  if (plotflag==2) postscript(tempfile4)
  x<-plot(m1data$total_floor_area,price,pch=".",
          xlab="total floor area (units = sq. m.)",
          ylab="price ( units = £ 1,000)")
  if (plotflag==1) dev.copy(postscript,tempfile4)
  dev.off()
}
