# Estimate model 2


# Load data
m2data<-read.csv(paste0(DataDir,"/m2data1.csv"))

attach(m2data)

# Define subsamples
qnt <- quantile(location_value, probs=c(.01, .99),na.rm=TRUE)
iFull <- location_value>qnt[1] & location_value<qnt[2]

# Estimate base model: Model 0
m2ols0<-lm(location_value~floodzone3+localplanrate
            +builtuparea_pct+restrictedland_pct
            +log(train_OX) + I(log(train_OX)*log(train_LON))
            +log(drive_OX)+I(log(drive_OX)^2)
            +log(train_CAM)+I(log(train_CAM)^2)
            +log(drive_CAM)+I(log(drive_CAM)^2)
            +log(train_LON)+I(log(train_LON)^2)+I(log(train_LON)^3)
            +I(log(popdensityOA)*(popdensityOA<=100))
            +I(log(popdensityOA)*(popdensityOA>100))
            +lu_domestic_shr+lu_gardens_shr+lu_road_shr+lu_rail_shr
            +lu_water_shr+lu_greenspace_shr
            +I(distance_LUT<10)+I(distance_STD<20),
            data=m2data,subset=iFull,na.action=na.exclude)
m2ols0$varlist<-c("location_value","floodzone3","localplanrate","builtuparea_pct", 
                     "restrictedland_pct","train_CAM","train_MK","train_OX","train_LON",
                     "drive_CAM","drive_MK","drive_OX","popdensityOA","lu_domestic_shr",
                     "lu_gardens_shr","lu_road_shr","lu_rail_shr","lu_water_shr","lu_greenspace_shr",
                     "distance_LUT","distance_STD")

summary(m2ols0)
p2ols0<-predict(m2ols0,na.action=na.exclude)
e2<-location_value[iFull]-p2ols0
y<-m2ols0$coefficients["log(train_LON)"]*log(train_LON) +
   m2ols0$coefficients["I(log(train_LON)^2)"]*log(train_LON)^2 +
   m2ols0$coefficients["I(log(train_LON)^3)"]*log(train_LON)^3 
plot(train_LON,y)  

map10<-heatmap1(latitude[iFull],longitude[iFull],e2,filter=!is.na(e2),
         resolution=0.001,zlabel="e2",outfile=paste0(OutDir,"/errs1.pdf"))
plot(m2ols0,pch='.')

save(m2ols0,file=paste0(OutDir,"/m2ols0.RData"))

m2ols1<-lm(location_value~floodzone3+localplanrate
             +builtuparea_pct+restrictedland_pct
             +spline_drive_CAM+spline_train_CAM
             +spline_drive_OX+spline_train_OX
             +spline_train_LON
             +I(train_LON*train_OX)
             +I(log(popdensityOA)*(popdensityOA<=100))
             +I(log(popdensityOA)*(popdensityOA>100))
             +lu_domestic_shr+lu_gardens_shr+lu_road_shr+lu_rail_shr
             +lu_water_shr+lu_greenspace_shr
             +I(distance_LUT<10)+I(distance_STD<20),
             data=m2data,subset=iFull,na.action=na.exclude)
summary(m2ols1)
itrain_CAM1<-grep("spline_train_CAM",names(m2ols1$coefficients))
itrain_CAM2<-grep("spline_train_CAM",colnames(m2data))
y1<-as.matrix(m2data[,itrain_CAM2]) %*% m2ols1$coefficients[itrain_CAM1]
plot(train_CAM,y1,pch=".")
idrive_CAM1<-grep("spline_drive_CAM",names(m2ols1$coefficients))
idrive_CAM2<-grep("spline_drive_CAM",colnames(m2data))
y2<-as.matrix(m2data[,idrive_CAM2]) %*% m2ols1$coefficients[idrive_CAM1]
plot(drive_CAM,y2,pch=".")

itrain_OX1<-grep("spline_train_OX",names(m2ols1$coefficients))
itrain_OX2<-grep("spline_train_OX",colnames(m2data))
y3<-as.matrix(m2data[,itrain_OX2]) %*% m2ols1$coefficients[itrain_OX1]
plot(train_OX,y3,pch=".")
idrive_OX1<-grep("spline_drive_OX",names(m2ols1$coefficients))
idrive_OX2<-grep("spline_drive_OX",colnames(m2data))
y4<-as.matrix(m2data[,idrive_OX2]) %*% m2ols1$coefficients[idrive_OX1]
plot(drive_OX,y4,pch=".")

