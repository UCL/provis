library(parallel)

no_cores<-detectCores()-1
cl<-makeCluster(no_cores,type="FORK")


houses<-m2data[,c("longitude","latitude")]
roads<-gIntersection(region_bbx[[r]],motorways)

clusterExport(cl,"houses")
clusterExport(cl,"roads")
n<-nrow(x)
n1<-floor(n/no_cores)
rem<-n-n1*no_cores
n2<-n1+1
i1<-seq(1,n2*rem,by=n2)
i2<-i1+n2-1
i3<-max(i2)+seq(1,n1*(no_cores-rem),by=n1)
i4<-i3+n1-1
i1<-c(i1,i3)
i2<-c(i2,i4)

getDist<-function(i1,i2,x,lines1) {
  y<-dist2Line(x[i1:i2,],lines1)
  return(y)
}

y<-parLapply(cl,1:no_cores,function(j1) getDist(i1[j1],i2[j1],houses,roads))

stopCluster(cl)

