#iMK<-(m2data$laname=="Milton Keynes")
#iOX<-(m2data$laname=="Oxford")
#iCAM<-(m2data$laname=="Cambridge")

#iCAM_dist<-(distance_CAM<=40)
#iOX_dist<-(distance_OX<=40)
#iMK_dist<-(distance_MK<=40)

# Nonparametric regression for Cambridge
CAM_data<-m2data[iCAM_dist,]
set.seed(100)
itemp<-sample(1:nrow(CAM_data),1000,replace=TRUE)
CAM_data<-CAM_data[itemp,]
np_CAM<-npreg(location_value~drive_CAM+train_CAM+train_london+drive_index+train_index,
              data=CAM_data)
plot(np_CAM)
#np_CAM<-npreg(bw_CAM)

# Linear model for Cambridge
m2_CAM<-lm(location_value~greenbelt+floodzone3+localplanrate
           +builtuparea_pct+restrictedland_pct
           +log(drive_CAM)
           +log(train_CAM)
           +log(drive_index)
           +log(popdensityOA)
           +I(log(popdensityOA)*(popdensityOA<6))
           +lu_domestic_shr+lu_gardens_shr+lu_road_shr+lu_rail_shr
           +lu_water_shr+lu_greenspace_shr
           +distance_STD,
           m2data,subset=iCAM_dist,na.action=na.exclude)
summary(m2_CAM)

p2_CAM<-predict(m2_CAM,na.action=na.exclude)
e2_CAM<-location_value[iCAM_dist]-p2_CAM
plot(m2_CAM)


## Nonparametric regression for MK
MK_data<-m2data[iMK_dist,]
set.seed(100)
itemp<-sample(1:nrow(CAM_data),1000,replace=TRUE)
MK_data<-MK_data[itemp,]
np_MK<-npreg(location_value~drive_MK+train_MK+train_london+drive_index+train_index,
             data=MK_data)
plot(np_MK)
#np_CAM<-npreg(bw_CAM)

# Linear model for Cambridge
m2_CAM<-lm(location_value~greenbelt+floodzone3+localplanrate
           +builtuparea_pct+restrictedland_pct
           +log(drive_CAM)
           +log(train_CAM)
           +log(drive_index)
           +log(popdensityOA)
           +I(log(popdensityOA)*(popdensityOA<6))
           +lu_domestic_shr+lu_gardens_shr+lu_road_shr+lu_rail_shr
           +lu_water_shr+lu_greenspace_shr
           +distance_STD,
           m2data,subset=iCAM_dist,na.action=na.exclude)
summary(m2_CAM)

p2_CAM<-predict(m2_CAM,na.action=na.exclude)
e2_CAM<-location_value[iCAM_dist]-p2_CAM
plot(m2_CAM)

# Estimate model for Oxford sub-sample

# Estimate model for MK subsample
iMK<-m2data$drive_MK<60 & iFull & m2data$laname=="Milton Keynes"
MKData<-m2data[iMK,]
i0<-sample(1:nrow(MKData),1000,replace=FALSE)
np1<-npreg(location_value~mintrain+train_MK+drive_index
           ,data=MKData,subset=i0)
plot(np1)


