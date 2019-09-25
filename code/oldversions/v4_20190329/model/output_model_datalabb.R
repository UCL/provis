m1<-lm(log1,data=m1data)
m1s<-summary(m1)
m1out<-data.frame(varname=names(m1$coefficients),val=m1$coefficients,
                  se=m1s$coefficients[,2])
write.csv(m1out,file='m1out.csv',row.names=FALSE)

m1cov<-m1s$cov.unscaled
write.csv(m1cov,file='m1cov.csv',row.names=FALSE,col.naames=FALSE)


