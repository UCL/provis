# Test splines

m1spline0<-crs(logprice~year+propertytype+newbuild+tenure+total_floor_area
                         +latitude+longitude,
                         data=m1data,basis="additive", kernel=FALSE,cv="none",
                         degree=m1spline1$degree,segments=m1spline1$segments,
                         lambda=m1spline1$lambda,include=m1spline1$include)


# Recovering g(e,n)
m1splinecoef<-matrix(m1spline0$model.lm$coefficients)
m1splinem<-m1spline0$model.lm$model
m1splinex<-m1splinem$P
m1splinex<-cbind(rep(1,dim(m1splinex)[1]),m1splinex)
hatvals<-m1splinex %*% m1splinecoef # check if you get the same fitted values
hatvals==m1spline0$fitted.values

sp_deg_area<-m1spline0$degree[1]
sp_seg_area<-m1spline0$segments[1]
sp_deg_lat<-m1spline0$degree[2]
sp_seg_lat<-m1spline0$segments[2]
sp_deg_lon<-m1spline0$degree[3]
sp_seg_lon<-m1spline0$segments[3]

logprice1 <- m1splinex[,(1:sp_deg_area+sp_seg_area)] %*% 
             m1splinecoef[(1:sp_deg_area+sp_seg_area)]
logprice2 <- m1splinex[,(sp_deg_area+sp_seg_area+1):(sp_deg_area+sp_seg_area+sp_deg_lat+sp_seg_lat-1)] %*% 
             m1splinecoef[(sp_deg_area+sp_seg_area+1):(sp_deg_area+sp_seg_area+sp_deg_lat+sp_seg_lat-1)]
logprice3 <- m1splinex[,(sp_deg_area+sp_seg_area+sp_deg_lat+sp_seg_lat):(sp_deg_area+sp_seg_area+sp_deg_lat+sp_seg_lat+sp_deg_lon+sp_seg_lon-2)] %*% 
             m1splinecoef[(sp_deg_area+sp_seg_area+sp_deg_lat+sp_seg_lat):(sp_deg_area+sp_seg_area+sp_deg_lat+sp_seg_lat+sp_deg_lon+sp_seg_lon-2)]


m1spline1$knots1<-quantile(m1data$total_floor_area,probs=seq(0.1,0.9,length.out=m1spline1$segments[1]))
m1spline1$knots2<-quantile(m1data$latitude,probs=seq(0.1,0.9,length.out=m1spline1$segments[2]))
m1spline1$knots3<-quantile(m1data$longitude,probs=seq(0.1,0.9,length.out=m1spline1$segments[3]))

m1data$bSize<-bSpline(m1data$total_floor_area,knots=m1spline1$knots1,degree=m1spline1$degree[1],intercept=FALSE)
m1data$bLat<-bSpline(m1data$latitude,knots=m1spline1$knots2,degree=m1spline1$degree[2],intercept=FALSE)
m1data$bLon<-bSpline(m1data$longitude,knots=m1spline1$knots3,degree=m1spline1$degree[3],intercept=FALSE)
m1spline3<-lm(logprice~year+propertytype+newbuild+tenure
                       +bSize+bLat+bLon,
                       data=m1data)

iSize1<-grep("bSize1",names(m1spline3$coefficients))
iLat1<-grep("bLat1\\>",names(m1spline3$coefficients))
iLon1<-grep("bLon1\\>",names(m1spline3$coefficients))

logprice1B<-m1data$bSize %*% 
            m1spline3$coefficients[iSize1:(iSize1+m1spline1$degree[1]+m1spline1$segments[1]-1)] 
logprice2B<-m1data$bLat %*%
            m1spline3$coefficients[iLat1:(iLat1+m1spline1$degree[2]+m1spline1$segments[2]-1)]
logprice3B<-m1data$bLon %*% 
            m1spline3$coefficients[iLon1:(iLon1+m1spline1$degree[3]+m1spline1$segments[3]-1)]


plot(total_floor_area,logprice1,pch=".",col="red")
points(total_floor_area,logprice1B,pch=".",col="green")

plot(latitude,logprice2,pch=".",col="red")
points(latitude,logprice2B,pch=".",col="green")

plot(longitude,logprice3,pch=".",col="red")
points(longitude,logprice3B,pch=".",col="green")

location_valueA<-logprice2+logprice3
location_valueB<-logprice2B+logprice3B
plot(location_valueA,location_valueB,pch=".")
lines(location_valueA,location_valueA,col="red")
